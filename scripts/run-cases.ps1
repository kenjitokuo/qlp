param(
  [Parameter(Mandatory = $true)]
  [string[]]$Models,

  [int]$MaxSol = 20,

  [Parameter(Mandatory = $true)]
  [string]$Out,

  [Parameter(Mandatory = $true)]
  [string]$Cases
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# 既存 out は消して、毎回クリーンに作る
if (Test-Path -LiteralPath $Out) {
  Remove-Item -LiteralPath $Out -Force
}

$caseLines = Get-Content -LiteralPath $Cases | Where-Object {
  ($_ -ne $null) -and ($_.Trim().Length -gt 0) -and (-not ($_.Trim() -match '^\s*#'))
}

foreach ($model in $Models) {
  foreach ($line in $caseLines) {
    $parts = $line -split "`t"
    if ($parts.Count -lt 2) {
      throw "Bad case line (expected: <qprog><TAB><goal>): $line"
    }

    $qprog = $parts[0].Trim()
    $goal  = $parts[1].Trim()

    & stack exec qlp -- --model $model --max-sol $MaxSol --compare-jsonl $Out $qprog $goal
    if ($LASTEXITCODE -ne 0) {
      throw "qlp failed (exit=$LASTEXITCODE) model=$model qprog=$qprog goal=$goal"
    }
  }
}

Write-Host "Wrote: $Out"
