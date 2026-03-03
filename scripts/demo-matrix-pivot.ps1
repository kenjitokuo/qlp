<#
.SYNOPSIS
  Pivot demo_matrix_table.tsv into a matrix:
  rows = case, columns = model, cell = verdict
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$In,
  [Parameter(Mandatory=$true)][string]$Out
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (!(Test-Path $In)) { throw "Input file not found: $In" }

$rows = Import-Csv $In -Delimiter "`t" | Where-Object { $_.case -and $_.model -and $_.verdict }

# preserve first-appearance order (stable, matches how JSONL was produced)
$caseOrder  = New-Object System.Collections.Generic.List[string]
$modelOrder = New-Object System.Collections.Generic.List[string]

foreach ($r in $rows) {
  if (-not $caseOrder.Contains($r.case))   { $caseOrder.Add($r.case) }
  if (-not $modelOrder.Contains($r.model)) { $modelOrder.Add($r.model) }
}

# build lookup: case -> model -> verdict
$map = @{}
foreach ($r in $rows) {
  if (-not $map.ContainsKey($r.case)) { $map[$r.case] = @{} }
  $map[$r.case][$r.model] = $r.verdict
}

# write TSV
$lines = New-Object System.Collections.Generic.List[string]
$header = "case" + "`t" + ($modelOrder -join "`t")
$lines.Add($header)

foreach ($c in $caseOrder) {
  $cells = New-Object System.Collections.Generic.List[string]
  $cells.Add($c)
  foreach ($m in $modelOrder) {
    if ($map.ContainsKey($c) -and $map[$c].ContainsKey($m)) {
      $cells.Add([string]$map[$c][$m])
    } else {
      $cells.Add("")
    }
  }
  $lines.Add(($cells -join "`t"))
}

[IO.File]::WriteAllLines($Out, $lines, [Text.UTF8Encoding]::new($false))
Write-Host ("Wrote: {0}" -f $Out)
