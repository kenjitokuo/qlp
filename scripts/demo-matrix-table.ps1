<#
.SYNOPSIS
  Convert compare-jsonl output (out_demo.jsonl) into a flat TSV table:
  case, model, verdict, comm_fail_pair
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$In,
  [Parameter(Mandatory=$true)][string]$Out
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (!(Test-Path $In)) { throw "Input file not found: $In" }

function Get-FileBaseNoExt([string]$p) {
  if ([string]::IsNullOrWhiteSpace($p)) { return "" }
  return [IO.Path]::GetFileNameWithoutExtension($p)
}

function Extract-CaseName([string]$qprogPath) {
  $base = Get-FileBaseNoExt $qprogPath   # e.g. qprog_A1_QR
  if ($base -match '^qprog_(.+)$') { return $Matches[1] }
  return $base
}

function Extract-AtomLetter([string]$atomText) {
  if ([string]::IsNullOrWhiteSpace($atomText)) { return "" }
  if ($atomText -match 'Atom\s+"([A-Z])"') { return $Matches[1] }
  return ""
}

function Extract-Pair([object]$pairObj) {
  if ($null -eq $pairObj) { return "" }
  # pairObj is expected to be an array with 2 strings
  try {
    $a = [string]$pairObj[0]
    $b = [string]$pairObj[1]
  } catch {
    return ""
  }
  $la = Extract-AtomLetter $a
  $lb = Extract-AtomLetter $b
  if ([string]::IsNullOrWhiteSpace($la) -or [string]::IsNullOrWhiteSpace($lb)) { return "" }
  return ($la + "||" + $lb)
}

# ---- Read JSONL ----
$records = Get-Content $In | Where-Object { $_ -and $_.Trim() } | ForEach-Object { $_ | ConvertFrom-Json }

# ---- Build rows ----
$rows = foreach ($r in $records) {
  if ($null -eq $r) { continue }

  $modelPath = [string]$r.model
  $qprogPath = [string]$r.qprog
  $verdict   = [string]$r.verdict

  if ([string]::IsNullOrWhiteSpace($modelPath) -or [string]::IsNullOrWhiteSpace($qprogPath) -or [string]::IsNullOrWhiteSpace($verdict)) {
    continue
  }

  $caseName  = Extract-CaseName $qprogPath
  $modelName = Get-FileBaseNoExt $modelPath

  $pair = Extract-Pair $r.comm_fail_pair
  $verdictCell = $verdict
  if ($verdict -eq "not-applicable" -and -not [string]::IsNullOrWhiteSpace($pair)) {
    $verdictCell = ("not-applicable({0})" -f $pair)
  }

  [PSCustomObject]@{
    case = $caseName
    model = $modelName
    verdict = $verdictCell
    comm_fail_pair = $pair
  }
}

# ---- TSV Output ----
$lines = New-Object System.Collections.Generic.List[string]
$lines.Add("case`tmodel`tverdict`tcomm_fail_pair")
foreach ($x in $rows) {
  $lines.Add(("{0}`t{1}`t{2}`t{3}" -f $x.case, $x.model, $x.verdict, $x.comm_fail_pair))
}

[IO.File]::WriteAllLines($Out, $lines, [Text.UTF8Encoding]::new($false))
Write-Host ("Wrote: {0}" -f $Out)
