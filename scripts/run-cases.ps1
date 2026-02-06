param(
  [string[]] $Models = @(".\tests\confArgA.conf", ".\tests\confArgB.conf"),
  [int]      $MaxSol = 5,
  [string]   $Out    = ".\out.jsonl",
  [string]   $Cases  = ".\cases.txt"
)

if (-not (Test-Path $Cases)) { throw "Cases file not found: $Cases" }

Remove-Item $Out -ErrorAction SilentlyContinue

foreach ($Model in $Models) {
  Get-Content $Cases | ForEach-Object {
    $line = $_.Trim()
    if ($line.Length -eq 0) { return }
    if ($line.StartsWith("#")) { return }

    $parts = $line -split "`t", 2
    if ($parts.Count -lt 2) { Write-Host "skip (bad line): $line"; return }

    $qprog = $parts[0].Trim()
    $goal  = $parts[1].Trim()

    stack exec qlp -- --model $Model --max-sol $MaxSol --compare-jsonl $Out $qprog $goal
  }
}

Write-Host "Wrote: $Out"
