<#
.SYNOPSIS
  Rebuild demo case C4_var_RS by cloning C2_var_QS and renaming predicate Q -> R.
  This guarantees the same syntax as an already-parseable file.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File .\scripts\make-C4-var-RS.ps1
#>

[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$srcQprog = ".\tests\demo\qprog_C2_var_QS.txt"
$srcGoal  = ".\tests\demo\goal_C2_var_QS.txt"
$dstQprog = ".\tests\demo\qprog_C4_var_RS.txt"
$dstGoal  = ".\tests\demo\goal_C4_var_RS.txt"

if (-not (Test-Path -LiteralPath $srcQprog)) { throw "Missing source qprog: $srcQprog" }
if (-not (Test-Path -LiteralPath $srcGoal))  { throw "Missing source goal:  $srcGoal" }

function Backup-IfExists([string]$p) {
  if (Test-Path -LiteralPath $p) {
    $ts = Get-Date -Format "yyyyMMdd_HHmmss"
    $bak = "$p.bak.$ts"
    Copy-Item -LiteralPath $p -Destination $bak -Force
    Write-Host ("Backed up: {0} -> {1}" -f $p, $bak)
  }
}

Backup-IfExists $dstQprog
Backup-IfExists $dstGoal

$qprogTxt = Get-Content -LiteralPath $srcQprog -Raw -Encoding UTF8
$goalTxt  = Get-Content -LiteralPath $srcGoal  -Raw -Encoding UTF8

# Targeted rename: Atom "Q" -> Atom "R"
$qprogTxt2 = $qprogTxt -replace 'Atom\s+"Q"', 'Atom "R"'
$goalTxt2  = $goalTxt  -replace 'Atom\s+"Q"', 'Atom "R"'

Set-Content -LiteralPath $dstQprog -Value $qprogTxt2 -Encoding UTF8 -NoNewline
Set-Content -LiteralPath $dstGoal  -Value $goalTxt2  -Encoding UTF8 -NoNewline

Write-Host ("Wrote: {0}" -f $dstQprog)
Write-Host ("Wrote: {0}" -f $dstGoal)
