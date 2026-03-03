<#
.SYNOPSIS
  Run demo matrix A (models × cases) and append JSONL via qlp --compare-jsonl.

.DESCRIPTION
  Each model can restrict which cases it runs, so that comm_fail_pair observed for that model is limited to the intended pair appearing in those cases.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File .\scripts\run-demo-matrix.ps1 -Out ".\out_demo.jsonl" -MaxSol 5
#>

[CmdletBinding()]
param(
  [string]$Out = ".\out_demo.jsonl",
  [int]$MaxSol = 5
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Assert-File([string]$Path) {
  if (-not (Test-Path -LiteralPath $Path)) { throw ("File not found: {0}" -f $Path) }
}

$cases = @(
  @{ name="A1_QR";     qprog=".\tests\demo\qprog_A1_QR.txt";     goal=".\tests\demo\goal_A1_QR.txt" },
  @{ name="C2_var_QS"; qprog=".\tests\demo\qprog_C2_var_QS.txt"; goal=".\tests\demo\goal_C2_var_QS.txt" },
  @{ name="A4_RS";     qprog=".\tests\demo\qprog_A4_RS.txt";     goal=".\tests\demo\goal_A4_RS.txt" }
)

foreach ($c in $cases) { Assert-File $c.qprog; Assert-File $c.goal }

$models = @(
  @{ label="confArgA"; path=".\tests\confArgA.conf"; runCases=@("A1_QR") },
  @{ label="confArgB"; path=".\tests\confArgB.conf"; runCases=@("A1_QR","C2_var_QS","A4_RS") },
  @{ label="all_commute"; path=".\tests\demo_models\conf_demo_all_commute.conf"; runCases=@("A1_QR","C2_var_QS","A4_RS") },
  @{ label="only_qr_noncomm"; path=".\tests\demo_models\conf_demo_only_qr_noncomm.conf"; runCases=@("A1_QR") },
  @{ label="only_qs_noncomm"; path=".\tests\demo_models\conf_demo_only_qs_noncomm.conf"; runCases=@("C2_var_QS") },
  @{ label="only_rs_noncomm"; path=".\tests\demo_models\conf_demo_only_rs_noncomm.conf"; runCases=@("A4_RS") },
  @{ label="mixed"; path=".\tests\demo_models\conf_demo_mixed.conf"; runCases=@("A1_QR","C2_var_QS") }
)

foreach ($m in $models) { Assert-File $m.path }

Remove-Item -LiteralPath $Out -ErrorAction SilentlyContinue

foreach ($c in $cases) {
  Write-Host ""
  Write-Host ("===== CASE {0} =====" -f $c.name)
  foreach ($m in $models) {
    if (($m.runCases -ne $null) -and (-not ($m.runCases -contains $c.name))) { continue }
    Write-Host ""
    Write-Host ("== MODEL {0} ({1}) ==" -f $m.path, $m.label)
    stack exec qlp -- --model $m.path --max-sol $MaxSol --compare-jsonl $Out $c.qprog $c.goal
  }
}

Write-Host ("Wrote: {0}" -f $Out)
