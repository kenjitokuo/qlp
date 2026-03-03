param(
  [string]$Model = ".\tests\conf_demo_ham_2q.conf",
  [string]$OutDir = ".\tests\demo",
  [string]$Case = "B_bad1",
  [string]$CommMode = "pauli",
  [int]$MaxSol = 20,
  [string[]]$Atoms = @("Q","R","S","T")
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Assert-File([string]$p) { if (-not (Test-Path $p)) { throw "File not found: $p" } }
function Ensure-Dir([string]$p) { New-Item -ItemType Directory -Force -Path $p | Out-Null }

Assert-File $Model
Ensure-Dir $OutDir

# ---- normalize Atoms ----
$Atoms = @($Atoms)
if ($Atoms.Count -eq 1 -and $Atoms[0] -match ',') { $Atoms = $Atoms[0].Split(',') }
$Atoms = @($Atoms | ForEach-Object { $_.Trim() } | Where-Object { $_ })
if ($Atoms.Count -eq 0) { throw "Atoms is empty" }

$qprogPath = Join-Path $OutDir ("qprog_{0}_bad_one_group.txt" -f $Case)
$goalPath  = Join-Path $OutDir ("goal_{0}_bad_one_group.txt"  -f $Case)
$outJsonl  = (".\out_demo_ham_{0}_bad_one_group.jsonl" -f $Case)

Remove-Item $qprogPath -Force -ErrorAction SilentlyContinue
Remove-Item $goalPath  -Force -ErrorAction SilentlyContinue
Remove-Item $outJsonl  -Force -ErrorAction SilentlyContinue

function AtomText([string]$p) { 'Atom "' + $p + '" [TFun "a" []]' }

# ---- QProgram (Haskell Show) ----
$clauses = @(
