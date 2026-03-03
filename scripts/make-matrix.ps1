[CmdletBinding()]
param([string]$In = ".\out_demo.jsonl", [string]$Out = ".\matrix_verdict.tsv")
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
$rows = Get-Content $In | ForEach-Object { $_ | ConvertFrom-Json }
$models = $rows | Select-Object -ExpandProperty model -Unique | Sort-Object
$cases = $rows | ForEach-Object { Split-Path $_.qprog -Leaf } | Sort-Object -Unique
$header = @("case") + $models
$lines = New-Object System.Collections.Generic.List[string]
$lines.Add(($header -join "`t")) | Out-Null
foreach ($c in $cases) {
  $row = New-Object System.Collections.Generic.List[string]
  $row.Add($c) | Out-Null
  foreach ($m in $models) {
    $hit = $rows | Where-Object { (Split-Path $_.qprog -Leaf) -eq $c -and $_.model -eq $m } | Select-Object -First 1
    $row.Add($hit.verdict) | Out-Null
  }
  $lines.Add(($row -join "`t")) | Out-Null
}
$lines | Set-Content -Encoding UTF8 $Out
Write-Host ("Wrote: {0}" -f $Out)
