<#
.SYNOPSIS
  Generate demo artifacts (comm_graph.tsv, demo_matrix_table.tsv, demo_matrix_pivot.tsv)
  from compare-jsonl output, with minimal sanity checks.

.USAGE
  pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\demo-report.ps1 -In .\out_demo.jsonl -OutDir .
  pwsh -NoProfile -NoProfile -ExecutionPolicy Bypass -File .\scripts\demo-report.ps1 -In .\out_demo.jsonl -OutDir . -FailOnMismatch
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory=$true)][string]$In,
  [Parameter(Mandatory=$true)][string]$OutDir,
  [switch]$FailOnMismatch
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (!(Test-Path -LiteralPath $In)) { throw "Input file not found: $In" }
if (!(Test-Path -LiteralPath $OutDir)) { New-Item -ItemType Directory -Path $OutDir | Out-Null }

function Get-CaseName([string]$qprogPath) {
  if ([string]::IsNullOrWhiteSpace($qprogPath)) { return "" }
  $base = [IO.Path]::GetFileNameWithoutExtension($qprogPath)
  return ($base -replace '^qprog_','')
}

function Get-ModelName([string]$modelPath) {
  if ([string]::IsNullOrWhiteSpace($modelPath)) { return "" }
  return [IO.Path]::GetFileNameWithoutExtension($modelPath)
}

function Get-PairKey($rec) {
  if ($null -eq $rec.comm_fail_pair) { return $null }
  if ($rec.comm_fail_pair.Count -lt 2) { return $null }
  $m1 = [regex]::Match([string]$rec.comm_fail_pair[0], 'Atom\s+"([^"]+)"')
  $m2 = [regex]::Match([string]$rec.comm_fail_pair[1], 'Atom\s+"([^"]+)"')
  if (!$m1.Success -or !$m2.Success) { return $null }
  return ($m1.Groups[1].Value + "||" + $m2.Groups[1].Value)
}

function Get-ExpectedPair([string]$case) {
  $m = [regex]::Match($case, '_([A-Z])([A-Z])$')
  if ($m.Success) { return ($m.Groups[1].Value + "||" + $m.Groups[2].Value) }
  return $null
}

function Get-VerdictLabel($rec) {
  $v = [string]$rec.verdict
  if ($v -eq "not-applicable") {
    $pair = Get-PairKey $rec
    if ($pair) { return ("not-applicable({0})" -f $pair) }
    return "not-applicable"
  }
  return $v
}

function Write-Tsv([string]$path, [string[]]$columns, [object[]]$rows) {
  $lines = New-Object System.Collections.Generic.List[string]
  $lines.Add(($columns -join "`t"))
  foreach ($r in $rows) {
    $vals = foreach ($c in $columns) {
      $v = $null
      try { $v = $r.$c } catch { $v = $null }
      if ($null -eq $v) { "" } else { ([string]$v -replace "(`r`n|`r|`n)", " ") }
    }
    $lines.Add(($vals -join "`t"))
  }
  [IO.File]::WriteAllLines($path, $lines, [Text.UTF8Encoding]::new($false))
}

# ---- Read JSONL ----
$records = @()
Get-Content -LiteralPath $In | ForEach-Object {
  if (![string]::IsNullOrWhiteSpace($_)) {
    try { $records += ($_ | ConvertFrom-Json) } catch { throw "Invalid JSONL line: $_" }
  }
}
$records = @($records | Where-Object { $_ -ne $null })

if ($records.Count -eq 0) { throw "No records loaded from: $In" }

# ---- Sanity: not-applicable rows should have comm_fail_pair ----
$na = @($records | Where-Object { $_.verdict -eq "not-applicable" })
$naNoPair = @($na | Where-Object { $null -eq $_.comm_fail_pair })
if ($naNoPair.Count -gt 0) {
  $msg = "not-applicable rows without comm_fail_pair exist: {0}" -f $naNoPair.Count
  if ($FailOnMismatch) { throw $msg } else { Write-Warning $msg }
}

# ---- Build comm_graph.tsv (per model) ----
$commRows = foreach ($g in ($na | Where-Object { $null -ne $_.comm_fail_pair } | Group-Object model)) {
  $modelName = Get-ModelName $g.Name
  $pairsAll = @($g.Group | ForEach-Object { Get-PairKey $_ } | Where-Object { $_ })
  $uniq = @($pairsAll | Sort-Object -Unique)
  [PSCustomObject]@{
    model = $modelName
    na_rows = @($g.Group).Count
    unique_noncomm_pairs = $uniq.Count
    pairs = ($uniq -join "; ")
  }
}
$commPath = Join-Path $OutDir "comm_graph.tsv"
Write-Tsv -path $commPath -columns @("model","na_rows","unique_noncomm_pairs","pairs") -rows @($commRows | Sort-Object model)

# ---- Build demo_matrix_table.tsv (case, model, verdict) ----
$tableRows = foreach ($r in $records) {
  $case = Get-CaseName ([string]$r.qprog)
  $model = Get-ModelName ([string]$r.model)
  if ([string]::IsNullOrWhiteSpace($case) -or [string]::IsNullOrWhiteSpace($model)) { continue }
  [PSCustomObject]@{
    case = $case
    model = $model
    verdict = (Get-VerdictLabel $r)
  }
}
$tablePath = Join-Path $OutDir "demo_matrix_table.tsv"
Write-Tsv -path $tablePath -columns @("case","model","verdict") -rows @($tableRows | Sort-Object case, model)

# ---- Build demo_matrix_pivot.tsv (pivot case x model) ----
$cases = @($tableRows | Select-Object -ExpandProperty case | Sort-Object -Unique)
$models = @($tableRows | Select-Object -ExpandProperty model | Sort-Object -Unique)

$index = @{}
foreach ($tr in $tableRows) {
  $k = "{0}@@{1}" -f $tr.case, $tr.model
  if (!$index.ContainsKey($k)) { $index[$k] = New-Object System.Collections.Generic.List[string] }
  $index[$k].Add([string]$tr.verdict)
}

$pivotRows = foreach ($c in $cases) {
  $row = [ordered]@{ case = $c }
  foreach ($m in $models) {
    $k = "{0}@@{1}" -f $c, $m
    if ($index.ContainsKey($k)) {
      $vals = @($index[$k] | Sort-Object -Unique)
      if ($vals.Count -eq 1) { $row[$m] = $vals[0] } else { $row[$m] = ($vals -join "; ") }
    } else {
      $row[$m] = ""
    }
  }
  [PSCustomObject]$row
}

# ---- Optional sanity: per-case comm_fail_pair should be unique and match suffix _XY when present ----
$bad = @()
foreach ($c in $cases) {
  $expected = Get-ExpectedPair $c
  if ($null -eq $expected) { continue }
  $pairs = @($na | Where-Object { (Get-CaseName $_.qprog) -eq $c } | ForEach-Object { Get-PairKey $_ } | Where-Object { $_ } | Sort-Object -Unique)
  if ($pairs.Count -eq 0) { continue }
  $ok = ($pairs.Count -eq 1 -and $pairs[0] -eq $expected)
  if (!$ok) {
    $bad += [PSCustomObject]@{ case = $c; expected = $expected; pairs = ($pairs -join "; "); unique_pairs = $pairs.Count }
  }
}

if ($bad.Count -gt 0) {
  $msg = "case -> comm_fail_pair mismatch exists: {0}" -f $bad.Count
  if ($FailOnMismatch) { throw $msg } else { Write-Warning $msg }
}

$pivotPath = Join-Path $OutDir "demo_matrix_pivot.tsv"
Write-Tsv -path $pivotPath -columns @("case") + $models -rows $pivotRows

Write-Host ("Wrote: {0}" -f $commPath)
Write-Host ("Wrote: {0}" -f $tablePath)
Write-Host ("Wrote: {0}" -f $pivotPath)

if ($bad.Count -gt 0 -and !$FailOnMismatch) {
  Write-Host ""
  Write-Host "---- mismatches (for inspection) ----"
  $bad | Sort-Object case | Format-Table -AutoSize | Out-String | Write-Host
}
