param(
  [Parameter(Mandatory=$true)][string]$Model,
  [Parameter(Mandatory=$true)][string]$OutJsonl,
  [string]$CommMode = "pauli",
  [int]$SizeK = 0,              # SizeK>0: サイズKの可換集合（クリーク）を列挙
  [switch]$Maximal,             # -Maximal: 極大クリーク（maximal cliques）を列挙
  [int]$MaxShow = 30,           # 画面表示する件数（JSONLは全件）
  [string[]]$Atoms = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$SCRIPT_VERSION = "2026-02-11e"

function Assert-File([string]$p) { if (-not (Test-Path $p)) { throw "File not found: $p" } }
Assert-File $Model

# ---- Atoms 正規化 ----
$Atoms = @($Atoms)
if ($Atoms.Length -eq 0) { throw "Atoms must be provided." }
if ($Atoms.Length -eq 1 -and $Atoms[0] -match ',') { $Atoms = $Atoms[0].Split(',') }
$Atoms = @($Atoms | ForEach-Object { $_.Trim() } | Where-Object { $_ })
if ($Atoms.Length -lt 2) { throw "Need at least 2 atoms." }

function Key([string]$a,[string]$b) {
  $aa = [string]$a; $bb = [string]$b
  if ([string]::CompareOrdinal($aa, $bb) -lt 0) { "$aa|$bb" } else { "$bb|$aa" }
}

# ---- サイズ取得（.Count は使わない）----
function Size-Enumerable($xs) {
  if ($null -eq $xs) { return 0 }
  $n = 0
  foreach ($x in $xs) { $n++ }
  return $n
}

# ---- HashSet utilities（.Count は使わない）----
function HS([string[]]$xs) {
  $h = [System.Collections.Generic.HashSet[string]]::new()
  if ($null -ne $xs) { foreach ($x in $xs) { [void]$h.Add([string]$x) } }
  return $h
}

function HS-Intersect([System.Collections.Generic.HashSet[string]]$A, [System.Collections.Generic.HashSet[string]]$B) {
  $h = [System.Collections.Generic.HashSet[string]]::new()
  if ($null -eq $A -or $null -eq $B) { return $h }
  foreach ($x in $A) { if ($B.Contains($x)) { [void]$h.Add($x) } }
  return $h
}

function HS-Union([System.Collections.Generic.HashSet[string]]$A, [System.Collections.Generic.HashSet[string]]$B) {
  $h = [System.Collections.Generic.HashSet[string]]::new()
  if ($null -ne $A) { foreach ($x in $A) { [void]$h.Add($x) } }
  if ($null -ne $B) { foreach ($x in $B) { [void]$h.Add($x) } }
  return $h
}

function HS-IsEmpty([System.Collections.Generic.HashSet[string]]$H) {
  if ($null -eq $H) { return $true }
  foreach ($x in $H) { return $false }
  return $true
}

# ---- comm-check キャッシュ ----
$commCache = @{}  # key "A|B" -> bool commute?
$adj = @{}        # atom -> HashSet(neighbors that commute)

foreach ($a in $Atoms) { $adj[$a] = [System.Collections.Generic.HashSet[string]]::new() }

function Get-Comm([string]$a,[string]$b) {
  if ($a -eq $b) { return $true }
  $k = Key $a $b
  if ($commCache.ContainsKey($k)) { return [bool]$commCache[$k] }

  $line = (stack exec qlp -- --model $Model --comm $CommMode comm-check $a $b | Where-Object { $_.Trim() } | Select-Object -Last 1)
  if (-not $line) { throw "comm-check produced no output for pair $a,$b" }
  $rec = $line | ConvertFrom-Json
  $comm = [bool]$rec.comm
  $commCache[$k] = $comm
  return $comm
}

Write-Host ("SCRIPT_VERSION: {0}" -f $SCRIPT_VERSION)
Write-Host ("Model: {0}" -f $Model)
Write-Host ("CommMode: {0}" -f $CommMode)
Write-Host ("Atoms: {0} (n={1})" -f ($Atoms -join ", "), $Atoms.Length)

# ---- 全ペア評価して隣接（可換）グラフを構築 ----
$noncommEdges = @()
for ($i=0; $i -lt $Atoms.Length; $i++) {
  for ($j=$i+1; $j -lt $Atoms.Length; $j++) {
    $a = [string]$Atoms[$i]; $b = [string]$Atoms[$j]
    $c = Get-Comm $a $b
    if ($c) {
      [void]$adj[$a].Add($b)
      [void]$adj[$b].Add($a)
    } else {
      $noncommEdges += (Key $a $b)
    }
  }
}

Write-Host ("NonComm edges: {0}" -f $noncommEdges.Length)

# ---- JSONL 初期化 ----
Remove-Item $OutJsonl -ErrorAction SilentlyContinue
New-Item -ItemType File -Force -Path $OutJsonl | Out-Null

function Write-Jsonl($obj) {
  $json = $obj | ConvertTo-Json -Depth 30 -Compress
  Add-Content -Encoding utf8NoBOM -LiteralPath $OutJsonl -Value ($json + "`n")
}

# =========================================================
# (A) サイズKの可換集合（クリーク）列挙
# =========================================================
function IntersectListWithSet([string[]]$xs, [System.Collections.Generic.HashSet[string]]$hs) {
  $r = @()
  if ($null -eq $xs -or $null -eq $hs) { return ,$r }
  foreach ($x in $xs) { if ($hs.Contains($x)) { $r += $x } }
  return ,$r
}

$found = 0
$shown = 0

function SearchK([string[]]$chosen, [string[]]$cand) {
  if ($SizeK -le 0) { return }

  if ($chosen.Length -eq $SizeK) {
    $script:found++
    $obj = @{
      kind = "clique_k"
      model = $Model
      comm_mode = $CommMode
      size = $chosen.Length
      clique = $chosen
      n_atoms = $Atoms.Length
      noncomm_edge_count = $noncommEdges.Length
    }
    Write-Jsonl $obj
    if ($script:shown -lt $MaxShow) {
      $script:shown++
      Write-Host ("[{0}] {1}" -f $script:shown, ($chosen -join ", "))
    }
    return
  }

  if ($chosen.Length + $cand.Length -lt $SizeK) { return }

  for ($i=0; $i -lt $cand.Length; $i++) {
    $v = [string]$cand[$i]
    $rest = @()
    if ($i+1 -lt $cand.Length) { $rest = $cand[($i+1)..($cand.Length-1)] }

    $nextCand = IntersectListWithSet $rest $adj[$v]
    if ($chosen.Length -gt 0) {
      foreach ($u in $chosen) { $nextCand = IntersectListWithSet $nextCand $adj[$u] }
    }
    SearchK (@($chosen + @($v))) $nextCand
  }
}

# =========================================================
# (B) 極大クリーク列挙（Bron–Kerbosch with pivot）
# =========================================================
function Choose-Pivot([System.Collections.Generic.HashSet[string]]$P, [System.Collections.Generic.HashSet[string]]$X) {
  $UX = HS-Union $P $X
  $best = $null
  $bestDeg = -1
  foreach ($u in $UX) {
    $deg = 0
    if ($null -ne $P) {
      foreach ($v in $P) {
        if ($adj.ContainsKey($u) -and $adj[$u] -ne $null -and $adj[$u].Contains($v)) { $deg++ }
      }
    }
    if ($deg -gt $bestDeg) { $bestDeg = $deg; $best = $u }
  }
  return $best
}

$foundMax = 0
$shownMax = 0

function BK {
  param(
    [string[]]$R,
    [System.Collections.Generic.HashSet[string]]$P,
    [System.Collections.Generic.HashSet[string]]$X
  )

  if ($null -eq $P) { $P = HS @() }
  if ($null -eq $X) { $X = HS @() }

  if ( (HS-IsEmpty $P) -and (HS-IsEmpty $X) ) {
    $clique = @($R)
    $script:foundMax++
    $obj = @{
      kind = "clique_maximal"
      model = $Model
      comm_mode = $CommMode
      size = $clique.Length
      clique = $clique
      n_atoms = $Atoms.Length
      noncomm_edge_count = $noncommEdges.Length
    }
    Write-Jsonl $obj
    if ($script:shownMax -lt $MaxShow) {
      $script:shownMax++
      Write-Host ("[{0}] {1}" -f $script:shownMax, ($clique -join ", "))
    }
    return
  }

  $u = Choose-Pivot $P $X
  $Nu = [System.Collections.Generic.HashSet[string]]::new()
  if ($null -ne $u -and $adj.ContainsKey($u) -and $adj[$u] -ne $null) { $Nu = $adj[$u] }

  $toExplore = @()
  foreach ($v in $P) { if (-not $Nu.Contains($v)) { $toExplore += [string]$v } }

  foreach ($v in $toExplore) {
    $Nv = [System.Collections.Generic.HashSet[string]]::new()
    if ($adj.ContainsKey($v) -and $adj[$v] -ne $null) { $Nv = $adj[$v] }

    $P2 = HS-Intersect $P $Nv
    $X2 = HS-Intersect $X $Nv

    $R2 = @($R + @($v))
    BK $R2 $P2 $X2

    [void]$P.Remove($v)
    [void]$X.Add($v)
  }
}

# ---- 実行モード判定 ----
if ($SizeK -gt 0 -and $Maximal) { throw "Specify either -SizeK or -Maximal, not both." }
if ($SizeK -le 0 -and -not $Maximal) { throw "Specify -SizeK (>=1) or -Maximal." }

if ($SizeK -gt 0) {
  Write-Host ("Mode: enumerate cliques of size K={0}" -f $SizeK)
  $order = @($Atoms | Sort-Object)
  SearchK @() $order
  Write-Host ("Found cliques(K={0}): {1}" -f $SizeK, $found)
  Write-Jsonl @{
    kind = "summary"
    mode = "clique_k"
    model = $Model
    comm_mode = $CommMode
    k = $SizeK
    found = $found
    n_atoms = $Atoms.Length
    noncomm_edge_count = $noncommEdges.Length
  }
} else {
  Write-Host "Mode: enumerate maximal cliques"
  $P0 = HS (@($Atoms | Sort-Object))
  $X0 = HS @()
  BK @() $P0 $X0
  Write-Host ("Found maximal cliques: {0}" -f $foundMax)
  Write-Jsonl @{
    kind = "summary"
    mode = "clique_maximal"
    model = $Model
    comm_mode = $CommMode
    found = $foundMax
    n_atoms = $Atoms.Length
    noncomm_edge_count = $noncommEdges.Length
  }
}

Write-Host ("Wrote: {0}" -f $OutJsonl)
