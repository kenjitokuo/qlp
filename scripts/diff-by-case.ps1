<#
.SYNOPSIS
  List cases where verdict differs between models for the same (qprog, goal).

.DESCRIPTION
  Reads a JSONL file (one JSON object per line) produced by run-cases.ps1 / qlp --compare-jsonl.
  Groups by (qprog, goal) and prints only groups whose verdict differs across models.
  Also prints a one-line repro command for each model record.

.PARAMETER In
  Path to the input JSONL file.

.EXAMPLE
  powershell -ExecutionPolicy Bypass -File .\scripts\diff-by-case.ps1 -In .\out.jsonl
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)]
  [Alias('Input', 'Path')]
  [string]$In
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

if (-not (Test-Path -LiteralPath $In)) {
  throw "Input file not found: $In"
}

function Quote-PS([string]$s) {
  return "'" + ($s -replace "'", "''") + "'"
}

$groups = @{}  # key: "qprog<TAB>goal" -> List[object]
$lineNo = 0

foreach ($line in Get-Content -LiteralPath $In) {
  $lineNo++
  if ([string]::IsNullOrWhiteSpace($line)) { continue }

  try {
    $obj = $line | ConvertFrom-Json
  } catch {
    throw "JSON parse error at line $lineNo in ${In}: $($_.Exception.Message)"
  }

  $qprog = [string]$obj.qprog
  $goal  = [string]$obj.goal
  $key   = $qprog + "`t" + $goal

  if (-not $groups.ContainsKey($key)) {
    $groups[$key] = New-Object System.Collections.Generic.List[object]
  }
  $null = $groups[$key].Add($obj)
}

$diffs = New-Object System.Collections.Generic.List[object]

foreach ($kv in $groups.GetEnumerator()) {
  $recs = $kv.Value
  $verdicts = @($recs | ForEach-Object { $_.verdict } | Sort-Object -Unique)

  if ($verdicts.Count -gt 1) {
    $first = $recs[0]
    $null = $diffs.Add([pscustomobject]@{
      qprog    = $first.qprog
      goal     = $first.goal
      verdicts = $verdicts
      records  = $recs
    })
  }
}

if ($diffs.Count -eq 0) {
  Write-Output "No differing verdicts found."
  exit 0
}

foreach ($d in $diffs) {
  Write-Output ""
  Write-Output ("qprog={0}" -f $d.qprog)
  Write-Output ("goal ={0}" -f $d.goal)
  Write-Output ("verdicts=[" + ($d.verdicts -join ", ") + "]")

  foreach ($r in ($d.records | Sort-Object model)) {
    $model = [string]$r.model
    $qprog = [string]$r.qprog
    $goal  = [string]$r.goal

    Write-Output ("  model={0} verdict={1}" -f $model, $r.verdict)

    # NOTE:
    # - Print a command that can be pasted and executed as-is (no "repro:" prefix).
    # - Always include --debug so the commutativity failure pair is shown when it is not-applicable.
    # - Use PowerShell-safe single-quote escaping via Quote-PS.
    $cmd = "stack exec qlp -- --debug --model {0} --comm hilbert --solve {1} {2}" -f (Quote-PS $model), (Quote-PS $qprog), (Quote-PS $goal)
    Write-Output ("    {0}" -f $cmd)
  }
}
