param(
  [Parameter(Mandatory=$true)][string]$Model,
  [Parameter(Mandatory=$true)][string]$OutJsonl,
  [Parameter(Mandatory=$true)][string]$OutDir,
  [Parameter(Mandatory=$true)][string]$Case,
  [string]$CommMode = "pauli",
  [int]$MaxSol = 20,
  [string]$PairTplQProg = ".\tests\demo\qprog_A1_QR.txt",
  [string]$PairTplGoal  = ".\tests\demo\goal_A1_QR.txt",
  [string[]]$Atoms = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Assert-File([string]$p) {
  if (-not (Test-Path $p)) { throw "File not found: $p" }
}

Assert-File $Model
Assert-File $PairTplQProg
Assert-File $PairTplGoal

New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

# ---- Atoms を必ず配列に正規化（StrictMode 対策） ----
$Atoms = @($Atoms)
if ($Atoms.Count -eq 0) { $Atoms = @("Q","R","S","T") }
if ($Atoms.Count -eq 1 -and $Atoms[0] -match ',') { $Atoms = $Atoms[0].Split(',') }
$Atoms = @($Atoms | ForEach-Object { $_.Trim() } | Where-Object { $_ })

function New-ReplacedFile {
  param([string]$src,[string]$dst,[string]$from1,[string]$to1,[string]$from2,[string]$to2)
  $s = Get-Content -Raw -Encoding utf8 $src

  # 連鎖置換防止（Q->R の後に R->S が Q由来のRにも当たる問題を回避）
  $tok1 = "__TMP_ATOM1__"
  $tok2 = "__TMP_ATOM2__"

  $s = $s -replace ('"' + [regex]::Escape($from1) + '"'), ('"' + $tok1 + '"')
  $s = $s -replace ('"' + [regex]::Escape($from2) + '"'), ('"' + $tok2 + '"')
  $s = $s -replace ('"' + [regex]::Escape($tok1) + '"'), ('"' + $to1 + '"')
  $s = $s -replace ('"' + [regex]::Escape($tok2) + '"'), ('"' + $to2 + '"')

  Set-Content -Encoding utf8NoBOM -Path $dst -Value $s
}

function ModelTag([string]$p) { [IO.Path]::GetFileNameWithoutExtension($p) }

# ペアログは Case×Model ごとに隔離（混線防止）
$pairDir = Join-Path $OutDir ("pairs_{0}_{1}" -f $Case, (ModelTag $Model))
New-Item -ItemType Directory -Force -Path $pairDir | Out-Null

Write-Host ("Atoms: {0} (count={1})" -f ($Atoms -join ", "), $Atoms.Count)

# ---- Key は常に ordinal で順序決定（比較のブレを除去）----
function Key([string]$a,[string]$b) {
  $aa = [string]$a; $bb = [string]$b
  if ([string]::CompareOrdinal($aa, $bb) -lt 0) { "$aa|$bb" } else { "$bb|$aa" }
}

# ---- comm-check 呼び出し（stack exec qlp -- で統一、最後の非空行(JSON)だけ拾う）----
function CommCheckLine([string]$a,[string]$b) {
  $line = (stack exec qlp -- --model $Model --comm $CommMode comm-check $a $b | Where-Object { $_.Trim() } | Select-Object -Last 1)
  if (-not $line) { throw "comm-check produced no output for pair $a,$b" }
  return $line
}

# --- (1) ペアごとの不可換を qlp(comm-check) で判定（テンプレ依存を排除） ---
$nonComm = @{}  # key "A|B" -> $true

for ($i=0; $i -lt $Atoms.Count; $i++) {
  for ($j=$i+1; $j -lt $Atoms.Count; $j++) {
    $a = [string]$Atoms[$i]
    $b = [string]$Atoms[$j]
    $tmp = Join-Path $pairDir ("out_pair_{0}_{1}.jsonl" -f $a,$b)
    Remove-Item $tmp -ErrorAction SilentlyContinue

    $line = CommCheckLine $a $b
    Set-Content -Encoding utf8NoBOM -Path $tmp -Value ($line + "`n")

    $rec = $line | ConvertFrom-Json
    if (-not $rec.comm) { $nonComm[(Key $a $b)] = $true }
  }
}

function NonComm([string]$a,[string]$b) {
  return $nonComm.ContainsKey((Key $a $b))
}

# ---- デバッグ表示：非可換エッジ一覧 ----
$edgeKeys = @($nonComm.Keys | Sort-Object)
Write-Host ("NonComm edges: {0}" -f ($edgeKeys.Count))
if ($edgeKeys.Count -gt 0) { Write-Host ("  " + ($edgeKeys -join ", ")) }

# --- (2) 最小グループ数（非可換グラフの彩色：branch & bound） ---
$V = @($Atoms)

$deg = @{}
foreach ($x in $V) {
  $d = 0
  foreach ($y in $V) { if ($x -ne $y -and (NonComm $x $y)) { $d++ } }
  $deg[$x] = $d
}

$ord = @($V | Sort-Object { -$deg[$_] }, { $_ })

function Clone-Classes($classes) {
  $r = @()
  foreach ($cl in @($classes)) { $r += ,(@($cl)) }
  return $r
}

function Greedy-Color($order) {
  $colorOf = @{}
  $classes = @()
  foreach ($v in @($order)) {
    $placed = $false
    for ($c=0; $c -lt $classes.Count; $c++) {
      $ok = $true
      foreach ($u in @($classes[$c])) { if (NonComm $v $u) { $ok = $false; break } }
      if ($ok) {
        $classes[$c] = @(@($classes[$c]) + @($v))
        $colorOf[$v] = $c
        $placed = $true
        break
      }
    }
    if (-not $placed) {
      $classes += ,(@($v))
      $colorOf[$v] = ($classes.Count-1)
    }
  }
  return @(
    ,$classes
    $colorOf
  )
}

# ---- PATCH 1: greedy の結果を保持（A2メタ用）----
$g = Greedy-Color $ord
$greedyClasses = $g[0]
$greedyK = $greedyClasses.Count
$greedyGroupSizes = @($greedyClasses | ForEach-Object { @($_).Count })

# best の初期値は greedy
$bestClasses = $g[0]
$bestK = $bestClasses.Count
$bestColorOf = $g[1]

# サニティ：エッジがあるのに bestK=1 はあり得ない
if ($edgeKeys.Count -gt 0 -and $bestK -lt 2) {
  $bestK = $V.Count
  $bestClasses = @()
  foreach ($v in $ord) { $bestClasses += ,(@($v)) }
  $bestColorOf = @{}
}

function Search([int]$k, $classes, $colorOf) {
  if ($k -ge $ord.Count) {
    if ($classes.Count -lt $script:bestK) {
      $script:bestK = $classes.Count
      $script:bestClasses = (Clone-Classes $classes)
      $script:bestColorOf = @{} + $colorOf
    }
    return
  }
  if ($classes.Count -ge $script:bestK) { return }

  $v = $ord[$k]

  for ($c=0; $c -lt $classes.Count; $c++) {
    $ok = $true
    foreach ($u in @($classes[$c])) { if (NonComm $v $u) { $ok = $false; break } }
    if ($ok) {
      $classes2 = (Clone-Classes $classes)
      $classes2[$c] = @(@($classes2[$c]) + @($v))
      $color2 = @{} + $colorOf
      $color2[$v] = $c
      Search ($k+1) $classes2 $color2
    }
  }

  if ($classes.Count + 1 -lt $script:bestK) {
    $classes3 = (Clone-Classes $classes) + ,(@($v))
    $color3 = @{} + $colorOf
    $color3[$v] = $classes.Count
    Search ($k+1) $classes3 $color3
  }
}

Search 0 @() @{}
$groups = $bestClasses

# --- (3) 分割結果から qprog/goal を生成（Haskell Show 形式） ---
$qprogPath = Join-Path $OutDir ("qprog_{0}_auto_groups.txt" -f $Case)
$goalPath  = Join-Path $OutDir ("goal_{0}_auto_groups.txt"  -f $Case)

function AtomText([string]$p) { 'Atom "' + $p + '" [TFun "a" []]' }

$clauses = @()

foreach ($p in $Atoms) {
  $clauses += 'Clause {negAtoms = [], posAtoms = [' + (AtomText $p) + ']}'
}

for ($i=0; $i -lt $groups.Count; $i++) {
  $gi = "G{0}" -f ($i+1)
  $neg = @(@($groups[$i]) | ForEach-Object { AtomText $_ })
  $clauses += 'Clause {negAtoms = [' + ($neg -join ", ") + '], posAtoms = [' + (AtomText $gi) + ']}'
}

$qprogText = "[`n  " + ($clauses -join ",`n  ") + "`n]`n"
Set-Content -Encoding utf8NoBOM -Path $qprogPath -Value $qprogText

$wantPos = @()
for ($i=0; $i -lt $groups.Count; $i++) { $wantPos += (AtomText ("G{0}" -f ($i+1))) }
$goalText = 'Goal {wantPos = [' + ($wantPos -join ", ") + '], wantNeg = []}' + "`n"
Set-Content -Encoding utf8NoBOM -Path $goalPath -Value $goalText

# --- (4) compare-jsonl を流して outJsonl を作成 → 最終行に A2 メタを付与して置換 ---
Remove-Item $OutJsonl -ErrorAction SilentlyContinue
New-Item -ItemType File -Force -Path $OutJsonl | Out-Null
stack exec qlp -- --model $Model --comm $CommMode --max-sol $MaxSol --compare-jsonl $OutJsonl $qprogPath $goalPath | Out-Null

$allLines = @(Get-Content -LiteralPath $OutJsonl)
$idxLast = -1
for ($i = $allLines.Count - 1; $i -ge 0; $i--) { if ($allLines[$i].Trim()) { $idxLast = $i; break } }
if ($idxLast -lt 0) { throw "OutJsonl has no JSON lines: $OutJsonl" }

$recFinal = ($allLines[$idxLast] | ConvertFrom-Json)

$kOpt = $groups.Count
$groupSizes = @($groups | ForEach-Object { @($_).Count })

$meta = [ordered]@{
  case = $Case
  model_tag = (ModelTag $Model)
  comm_mode = $CommMode
  n_atoms = $Atoms.Count
  atoms = @($Atoms)
  noncomm_edge_count = $edgeKeys.Count
  noncomm_edges = @($edgeKeys)
  k_opt = $kOpt
  group_sizes = @($groupSizes)
  k_greedy = $greedyK
  greedy_group_sizes = @($greedyGroupSizes)
}

# 既存の最終レコードに meta をマージ（同名キーは meta 側で上書き）
$merged = [ordered]@{}
$recFinal.PSObject.Properties | ForEach-Object { $merged[$_.Name] = $_.Value }
$meta.Keys | ForEach-Object { $merged[$_] = $meta[$_] }

$allLines[$idxLast] = ($merged | ConvertTo-Json -Depth 20 -Compress)
Set-Content -Encoding utf8NoBOM -LiteralPath $OutJsonl -Value ($allLines -join "`n" + "`n")

$recFinal2 = ($allLines[$idxLast] | ConvertFrom-Json)

Write-Host ("Groups (k={0}):" -f $groups.Count)
for ($i=0; $i -lt $groups.Count; $i++) { Write-Host ("  G{0}: {1}" -f ($i+1), ((@($groups[$i])) -join ", ")) }
Write-Host ("Pair logs: {0}" -f $pairDir)
Write-Host ("Wrote: {0}" -f $qprogPath)
Write-Host ("Wrote: {0}" -f $goalPath)
Write-Host ("Wrote: {0}  verdict={1}" -f $OutJsonl, $recFinal2.verdict)
