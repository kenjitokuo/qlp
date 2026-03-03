param(
  [Parameter(Mandatory=$true)][string]$Model,
  [Parameter(Mandatory=$true)][string]$ContextsJsonl,
  [Parameter(Mandatory=$true)][string]$OutDir,
  [Parameter(Mandatory=$true)][string]$Case,
  [string]$CommModeReal = "pauli",
  [string]$CommModeBase = "always",
  [int]$MaxSol = 1,
  [string[]]$Atoms = @()
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$SCRIPT_VERSION = "2026-02-11f"

function Assert-File([string]$p) { if (-not (Test-Path $p)) { throw "File not found: $p" } }
function Assert-Dir([string]$p) { New-Item -ItemType Directory -Force -Path $p | Out-Null }

function Len($xs) { $n = 0; foreach ($x in @($xs)) { $n++ }; return $n }

# ---- 入力 ----
Assert-File $Model
Assert-File $ContextsJsonl
Assert-Dir  $OutDir

# ---- Atoms 正規化 ----
$Atoms = @($Atoms)
if ((Len $Atoms) -eq 0) { throw "Atoms must be provided." }
if ((Len $Atoms) -eq 1 -and [string]$Atoms[0] -match ',') { $Atoms = [string]$Atoms[0].Split(',') }
$Atoms = @($Atoms | ForEach-Object { [string]$_ } | ForEach-Object { $_.Trim() } | Where-Object { $_ })

if ((Len $Atoms) -lt 2) { throw "Need at least 2 atoms." }

# ---- contexts JSONL 読み取り（kind=clique_k, clique配列）----
$contexts = @()

$lnNo = 0
foreach ($ln in (Get-Content -LiteralPath $ContextsJsonl)) {
  $lnNo++
  if (-not ($ln -and $ln.Trim())) { continue }
  $obj = $null
  try { $obj = ($ln | ConvertFrom-Json) } catch { continue }
  if ($null -eq $obj) { continue }

  $hasKind = $false
  $hasClique = $false
  foreach ($pn in $obj.PSObject.Properties.Name) {
    if ($pn -eq "kind")  { $hasKind = $true }
    if ($pn -eq "clique") { $hasClique = $true }
  }
  if (-not $hasKind -or -not $hasClique) { continue }
  if ([string]$obj.kind -ne "clique_k") { continue }

  $cl = @()
  foreach ($x in @($obj.clique)) { $cl += ,([string]$x) }
  $cl = @($cl | ForEach-Object { $_.Trim() } | Where-Object { $_ })
  if ((Len $cl) -ge 2) { $contexts += ,(@($cl)) }
}

if ((Len $contexts) -eq 0) { throw "No clique_k records found in $ContextsJsonl" }

# ---- comm-check キャッシュ ----
function Key([string]$a,[string]$b) {
  $aa = [string]$a; $bb = [string]$b
  if ([string]::CompareOrdinal($aa, $bb) -lt 0) { "$aa|$bb" } else { "$bb|$aa" }
}

$commCache = @{}  # "A|B" -> bool

function Get-Comm([string]$a,[string]$b) {
  if ($a -eq $b) { return $true }
  $k = Key $a $b
  if ($commCache.ContainsKey($k)) { return [bool]$commCache[$k] }

  $line = (stack exec qlp -- --model $Model --comm $CommModeReal comm-check $a $b | Where-Object { $_.Trim() } | Select-Object -Last 1)
  if (-not $line) { throw "comm-check produced no output for pair $a,$b" }

  $rec = $line | ConvertFrom-Json
  $comm = [bool]$rec.comm
  $commCache[$k] = $comm
  return $comm
}

# ---- ある context に対して breaker を選ぶ（少なくとも1ペア非可換）----
function In-List([string]$x, [string[]]$xs) {
  foreach ($y in @($xs)) { if ([string]$y -eq [string]$x) { return $true } }
  return $false
}

function Find-Breaker([string[]]$ctx) {
  $ctxSorted = @($ctx | Sort-Object)
  $atomsSorted = @($Atoms | Sort-Object)

  foreach ($b in $atomsSorted) {
    if (In-List $b $ctxSorted) { continue }
    foreach ($c in $ctxSorted) {
      if (-not (Get-Comm $b $c)) {
        return @($b, "$b|$c")
      }
    }
  }
  return @($null, $null)
}

# ---- QLP 入力生成（Haskell Show 形式）----
function AtomText([string]$p) { 'Atom "' + $p + '" [TFun "a" []]' }

function Write-QProgGoal([string[]]$body, [string]$head, [string]$qprogPath, [string]$goalPath) {
  $clauses = @()

  foreach ($p in $body) {
    $clauses += 'Clause {negAtoms = [], posAtoms = [' + (AtomText $p) + ']}'
  }

  $neg = @()
  foreach ($p in $body) { $neg += (AtomText $p) }

  $clauses += 'Clause {negAtoms = [' + ($neg -join ", ") + '], posAtoms = [' + (AtomText $head) + ']}'

  $qprogText = "[`n  " + ($clauses -join ",`n  ") + "`n]`n"
  Set-Content -Encoding utf8NoBOM -Path $qprogPath -Value $qprogText

  $goalText = 'Goal {wantPos = [' + (AtomText $head) + '], wantNeg = []}' + "`n"
  Set-Content -Encoding utf8NoBOM -Path $goalPath -Value $goalText
}

# ---- 実行 ----
Write-Host ("SCRIPT_VERSION: {0}" -f $SCRIPT_VERSION)
Write-Host ("Model: {0}" -f $Model)
Write-Host ("CommModeReal: {0}" -f $CommModeReal)
Write-Host ("CommModeBase: {0}" -f $CommModeBase)
Write-Host ("Atoms: {0} (n={1})" -f ($Atoms -join ", "), (Len $Atoms))
Write-Host ("Contexts (from JSONL): n={0}" -f (Len $contexts))

$chosenCtx = $null
$breaker = $null
$failPair = $null

# “最小”に寄せるなら body が短い方から探す（JSONLが全部k=4でも将来の拡張に効く）
$contexts2 = @($contexts | Sort-Object { (Len $_) }, { ($_ -join "|") })

foreach ($ctx in $contexts2) {
  $r = Find-Breaker $ctx
  $b = $r[0]
  $fp = $r[1]
  if ($b) {
    $chosenCtx = @($ctx)
    $breaker = [string]$b
    $failPair = [string]$fp
    break
  }
}

if (-not $chosenCtx) { throw "No separating candidate found: every context commuted with every remaining atom (unexpected)." }

$head = "OK"
$body = @()
foreach ($x in @($chosenCtx)) { $body += ,([string]$x) }
$body += ,([string]$breaker)

$qprogPath = Join-Path $OutDir ("qprog_{0}_auto_from_contexts.txt" -f $Case)
$goalPath  = Join-Path $OutDir ("goal_{0}_auto_from_contexts.txt"  -f $Case)

Write-QProgGoal $body $head $qprogPath $goalPath

# compare-jsonl（base/real）
$outBase = Join-Path $OutDir ("out_{0}_{1}.jsonl" -f $Case, $CommModeBase)
$outReal = Join-Path $OutDir ("out_{0}_{1}.jsonl" -f $Case, $CommModeReal)

Remove-Item $outBase -ErrorAction SilentlyContinue
Remove-Item $outReal -ErrorAction SilentlyContinue

stack exec qlp -- --model $Model --comm $CommModeBase --max-sol $MaxSol --compare-jsonl $outBase $qprogPath $goalPath | Out-Null
stack exec qlp -- --model $Model --comm $CommModeReal --max-sol $MaxSol --compare-jsonl $outReal $qprogPath $goalPath | Out-Null

# 最終行表示
$lastBase = (Get-Content -LiteralPath $outBase | Where-Object { $_.Trim() } | Select-Object -Last 1)
$lastReal = (Get-Content -LiteralPath $outReal | Where-Object { $_.Trim() } | Select-Object -Last 1)

Write-Host ("Chosen context: {0}" -f (($chosenCtx | Sort-Object) -join ", "))
Write-Host ("Breaker: {0}  failPair(sample)={1}" -f $breaker, $failPair)
Write-Host ("Base last: {0}" -f $lastBase)
Write-Host ("Real last: {0}" -f $lastReal)

Write-Host ("Wrote: {0}" -f $qprogPath)
Write-Host ("Wrote: {0}" -f $goalPath)
Write-Host ("Wrote: {0}" -f $outBase)
Write-Host ("Wrote: {0}" -f $outReal)
