param(
  [Parameter(Mandatory = $true)]
  [string]$Out
)

if (-not (Test-Path -LiteralPath $Out)) {
  Write-Error "File not found: $Out"
  exit 1
}

$objs = @()
$lineNo = 0

Get-Content -LiteralPath $Out -Encoding UTF8 | ForEach-Object {
  $lineNo++
  $ln = $_
  if ([string]::IsNullOrWhiteSpace($ln)) { return }

  try {
    $o = $ln | ConvertFrom-Json
    if ($null -ne $o) { $objs += $o }
  } catch {
    Write-Warning ("Skip line {0}: JSON parse failed: {1}" -f $lineNo, $_.Exception.Message)
  }
}

if ($objs.Count -eq 0) {
  Write-Output "No JSON objects loaded."
  exit 0
}

$groups = $objs | Group-Object -Property verdict | Sort-Object Count -Descending

$groups | Select-Object Name, Count | Format-Table -AutoSize

foreach ($g in $groups) {
  ""
  ("{0}:" -f $g.Name) | Write-Output

  foreach ($o in $g.Group) {
    $model = $o.model
    $qprog = $o.qprog
    $goal  = $o.goal
    $vars  = if ($o.vars) { ($o.vars -join ",") } else { "" }

    ("- model={0} qprog={1} goal={2} vars=[{3}]" -f $model, $qprog, $goal, $vars) | Write-Output

    $sh = if ($o.sols_hilbert) { ($o.sols_hilbert -join "; ") } else { "" }
    $sa = if ($o.sols_always)  { ($o.sols_always  -join "; ") } else { "" }

    if ($sh -ne "") { ("  sols_hilbert: {0}" -f $sh) | Write-Output }
    if ($sa -ne "") { ("  sols_always : {0}" -f $sa) | Write-Output }
  }
}
