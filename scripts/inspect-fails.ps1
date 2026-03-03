[CmdletBinding()]
param([string]$In = ".\out_demo.jsonl", [int]$Top = 50)
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (!(Test-Path $In)) { throw "not found: $In" }

$rows = Get-Content $In | ForEach-Object { $_ | ConvertFrom-Json }

$na = $rows | Where-Object { $_.verdict -eq "not-applicable" -and $_.comm_fail_pair -ne $null }

Write-Host ("not-applicable rows with comm_fail_pair: {0}" -f $na.Count)

# (case, model, pair) の一覧（上位だけ）
Write-Host ""
Write-Host "Samples:"
$na | Select-Object -First ([Math]::Min($Top, $na.Count)) | ForEach-Object {
  $case  = [System.IO.Path]::GetFileNameWithoutExtension($_.qprog)
  $model = [System.IO.Path]::GetFileName($_.model)
  $p0 = $_.comm_fail_pair[0]
  $p1 = $_.comm_fail_pair[1]
  "{0}`t{1}`t{2}`t{3}" -f $case, $model, $p0, $p1
}

# ペア頻度
Write-Host ""
Write-Host "Top comm_fail_pair (by frequency):"
$na |
  ForEach-Object { ($_.comm_fail_pair[0] + " || " + $_.comm_fail_pair[1]) } |
  Group-Object |
  Sort-Object Count -Descending |
  Select-Object -First $Top |
  ForEach-Object { "{0}`t{1}" -f $_.Count, $_.Name }

# ケース×ペア（どのケースで起きたか）
Write-Host ""
Write-Host "Pairs by case:"
$na |
  Group-Object { [System.IO.Path]::GetFileNameWithoutExtension($_.qprog) } |
  Sort-Object Name |
  ForEach-Object {
    $case = $_.Name
    $pairs = $_.Group |
      ForEach-Object { ($_.comm_fail_pair[0] + " || " + $_.comm_fail_pair[1]) } |
      Group-Object |
      Sort-Object Count -Descending |
      Select-Object -First 10 |
      ForEach-Object { ("{0}x {1}" -f $_.Count, $_.Name) }
    "{0}`n  {1}`n" -f $case, ($pairs -join "`n  ")
  }
