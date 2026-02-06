param(
  [string] $Out = ".\out.jsonl"
)

if (-not (Test-Path $Out)) { throw "Out file not found: $Out" }

$rows = Get-Content $Out | ForEach-Object { $_ | ConvertFrom-Json }

$rows | Group-Object verdict | Sort-Object Count -Descending | Select-Object Name,Count

"`nnot-applicable:"
$rows | Where-Object verdict -eq "not-applicable" | Select-Object model,qprog,goal

"`nprovable-nonground:"
$rows | Where-Object verdict -eq "provable-nonground" | Select-Object model,qprog,goal
