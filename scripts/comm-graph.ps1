<#
.SYNOPSIS
  Build commutativity-failure summary per model from compare-jsonl output.
#>

[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)][string]$In,
  [Parameter(Mandatory = $true)][string]$Out
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (!(Test-Path -LiteralPath $In)) { throw "Input file not found: $In" }

# ---- Read JSONL (always array) ----
$records = @(
  Get-Content -LiteralPath $In |
    Where-Object { $_ -and $_.Trim() } |
    ForEach-Object { $_ | ConvertFrom-Json }
)

# ---- Filter + Group ----
$na = @(
  $records |
    Where-Object { $_.verdict -eq "not-applicable" -and $_.comm_fail_pair }
)

$groups = @(
  $na | Group-Object -Property model
)

# ---- Aggregate rows (always array) ----
$rows = @(
  foreach ($g in $groups) {
    $pairs = @(
      $g.Group | ForEach-Object {
        $a = ($_.comm_fail_pair[0] -replace 'Atom "([A-Z])".*', '$1')
        $b = ($_.comm_fail_pair[1] -replace 'Atom "([A-Z])".*', '$1')
        "$a||$b"
      }
    )

    $uniq = @($pairs | Sort-Object -Unique)

    $leaf = Split-Path -Path $g.Name -Leaf
    $base = [IO.Path]::GetFileNameWithoutExtension($leaf)

    [PSCustomObject]@{
      model = $base
      na_rows = $g.Count
      unique_noncomm_pairs = $uniq.Count
      pairs = ($uniq -join "; ")
    }
  }
)

# ---- Ensure output dir exists ----
$outDir = Split-Path -Path $Out -Parent
if ($outDir -and !(Test-Path -LiteralPath $outDir)) {
  New-Item -ItemType Directory -Path $outDir -Force | Out-Null
}

# ---- TSV Output (robust) ----
$lines = New-Object System.Collections.Generic.List[string]
$lines.Add("model`tna_rows`tunique_noncomm_pairs`tpairs")

foreach ($r in $rows) {
  $pairsField = if ($null -eq $r.pairs) { "" } else { $r.pairs }
  $lines.Add(("{0}`t{1}`t{2}`t{3}" -f $r.model, $r.na_rows, $r.unique_noncomm_pairs, $pairsField))
}

[IO.File]::WriteAllLines($Out, $lines, [Text.UTF8Encoding]::new($false))
Write-Host ("Wrote: {0}" -f $Out)
