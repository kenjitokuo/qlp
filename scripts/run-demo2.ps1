param(
  [string]$Model    = ".\tests\conf_demo_ham_2q.conf",
  [string]$OutDir   = ".\tests\demo",
  [string]$Case     = "B_auto4",
  [string]$OutJsonl = ".\out_demo_ham_auto.jsonl",
  [string]$CommMode = "pauli",
  [int]$MaxSol      = 20,
  [string[]]$Atoms  = @("Q","R","S","T")
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Assert-File([string]$p) { if (-not (Test-Path $p)) { throw "File not found: $p" } }
function Ensure-Dir([string]$p)  { New-Item -ItemType Directory -Force -Path $p | Out-Null }

# --- normalize atoms (support: -Atoms Q,R,S,T or -Atoms Q R S T) ---
$Atoms = @($Atoms)
if ($Atoms.Count -eq 1 -and $Atoms[0] -match ',') { $Atoms = $Atoms[0].Split(',') }
$Atoms = @($Atoms | ForEach-Object { $_.Trim() } | Where-Object { $_ })

Assert-File $Model
Ensure-Dir $OutDir

Write-Host ""
Write-Host "=== demo2a ==="
Write-Host ("Model: {0}" -f $Model)
Write-Host ("CommMode: {0}" -f $CommMode)
Write-Host ("Atoms: {0}" -f ($Atoms -join ", "))

# paths
$pairDir   = Join-Path $OutDir ("pairs_{0}_{1}" -f $Case, ([IO.Path]::GetFileNameWithoutExtension($Model)))
$qprogPath = Join-Path $OutDir ("qprog_{0}_auto_groups.txt" -f $Case)
$goalPath  = Join-Path $OutDir ("goal_{0}_auto_groups.txt"  -f $Case)
$readmePath = Join-Path $OutDir "README_demo2.md"

# clean (idempotent)
Remove-Item $pairDir   -Recurse -Force -ErrorAction SilentlyContinue
Remove-Item $OutJsonl  -Force -ErrorAction SilentlyContinue
Remove-Item $qprogPath -Force -ErrorAction SilentlyContinue
Remove-Item $goalPath  -Force -ErrorAction SilentlyContinue
Remove-Item $readmePath -Force -ErrorAction SilentlyContinue

# driver
$driver = ".\scripts\auto-group-ham.ps1"
Assert-File $driver

# IMPORTANT: call driver in-process so -Atoms is passed as string[] correctly
& $driver `
  -Model $Model `
  -OutJsonl $OutJsonl `
  -OutDir $OutDir `
  -Case $Case `
  -CommMode $CommMode `
  -MaxSol $MaxSol `
  -Atoms $Atoms

Assert-File $qprogPath
Assert-File $goalPath
Assert-File $OutJsonl

# Read group info from generated goal/qprog (lightweight, no extra parsing gymnastics)
$goalLine = (Get-Content $goalPath -Raw -Encoding utf8).Trim()

# Pair comm-check logs
$pairLines = @()
if (Test-Path $pairDir) {
  $pairLines = Get-ChildItem $pairDir -Filter "out_pair_*.jsonl" | Sort-Object Name | Get-Content
}

# Final verdict line
$finalLine = (Get-Content $OutJsonl | Where-Object { $_.Trim() } | Select-Object -Last 1)

# Write README (use SINGLE quotes for ``` fences to avoid PowerShell backtick escaping)
$lines = New-Object System.Collections.Generic.List[string]

$lines.Add('# Demo2: Hamiltonian term grouping by commutativity')
$lines.Add('')
$lines.Add('This demo groups Hamiltonian terms into commuting sets (useful for measurement grouping / Trotterization).')
$lines.Add('')
$lines.Add('## Run')
$lines.Add('')
$lines.Add('```powershell')
$lines.Add(('pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\run-demo2.ps1 -Model {0} -Case {1} -CommMode {2} -Atoms {3}' -f $Model, $Case, $CommMode, ($Atoms -join ",")))
$lines.Add('```')
$lines.Add('')
$lines.Add('## Inputs')
$lines.Add('')
$lines.Add(('* Model: `{0}`' -f $Model))
$lines.Add(('* CommMode: `{0}`' -f $CommMode))
$lines.Add(('* Atoms: `{0}`' -f ($Atoms -join ", ")))
$lines.Add('')
$lines.Add('## Generated files')
$lines.Add('')
$lines.Add(('* QProgram: `{0}`' -f $qprogPath))
$lines.Add(('* Goal: `{0}`' -f $goalPath))
$lines.Add(('* OutJsonl: `{0}`' -f $OutJsonl))
$lines.Add('')
$lines.Add('## Pair commutativity (from `qlp comm-check`)')
$lines.Add('')
$lines.Add('```json')
foreach ($pl in $pairLines) { if ($pl.Trim()) { $lines.Add($pl.Trim()) } }
$lines.Add('```')
$lines.Add('')
$lines.Add('## Final verdict (last JSONL line)')
$lines.Add('')
$lines.Add('```json')
$lines.Add($finalLine.Trim())
$lines.Add('```')
$lines.Add('')
$lines.Add('## Goal (for reference)')
$lines.Add('')
$lines.Add('```text')
$lines.Add($goalLine)
$lines.Add('```')

Set-Content -Encoding utf8NoBOM -Path $readmePath -Value (($lines -join "`n") + "`n")

Write-Host ""
Write-Host "---- wrote ----"
Write-Host $readmePath
Write-Host ""
Write-Host "---- show final verdict line ----"
Write-Host $finalLine
