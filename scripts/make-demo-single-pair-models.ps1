<#
.SYNOPSIS
  Create demo models that trigger comm_fail_pair for a single intended pair in the tested case set.

.DESCRIPTION
  These models are intentionally simple (q(a), r(a), s(a), t(a) assignments).
  Note: A model may have additional non-commuting pairs in general, but the demo matrix will run each model only on the case(s) that mention the intended pair.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File .\scripts\make-demo-single-pair-models.ps1
#>

[CmdletBinding()]
param(
  [string]$Dir = ".\tests\demo_models"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Backup-IfExists([string]$Path) {
  if (Test-Path -LiteralPath $Path) {
    $ts = Get-Date -Format "yyyyMMdd_HHmmss"
    $bak = "$Path.bak.$ts"
    Copy-Item -LiteralPath $Path -Destination $bak -Force
    Write-Host ("Backed up: {0} -> {1}" -f $Path, $bak)
  }
}

function Write-Conf([string]$Path, [string]$Comment, [string]$Q, [string]$R, [string]$S, [string]$T) {
  Backup-IfExists $Path
  $content = @(
    ("# demo model: {0}" -f $Comment),
    ("q(a) = {0}" -f $Q),
    ("r(a) = {0}" -f $R),
    ("s(a) = {0}" -f $S),
    ("t(a) = {0}" -f $T),
    ""
  ) -join "`r`n"
  $null = New-Item -ItemType Directory -Force -Path $Dir
  Set-Content -LiteralPath $Path -Value $content -Encoding UTF8
  Write-Host ("Wrote: {0}" -f $Path)
}

$onlyQr = Join-Path $Dir "conf_demo_only_qr_noncomm.conf"
$onlyQs = Join-Path $Dir "conf_demo_only_qs_noncomm.conf"
$onlyRs = Join-Path $Dir "conf_demo_only_rs_noncomm.conf"

Write-Conf -Path $onlyQr -Comment "only Q vs R differs (use with CASE A1_QR)" -Q "ket0" -R "ketplus" -S "ket0" -T "ket0"
Write-Conf -Path $onlyQs -Comment "only Q vs S differs (use with CASE C2_var_QS)" -Q "ket0" -R "ket0" -S "ketplus" -T "ket0"
Write-Conf -Path $onlyRs -Comment "only R vs S differs (use with CASE A4_RS)" -Q "ket0" -R "ket0" -S "ketplus" -T "ket0"
