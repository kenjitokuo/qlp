# stabilizer-search.ps1
# Usage examples:
#   pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\stabilizer-search.ps1 -N 3 -Stab ZZI IZZ -Top 40
#   pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\stabilizer-search.ps1 -N 3 -Stab ZZI IZZ -Top 40 -CommMode pauli -Model .\tests\conf_demo_pauli_3q.conf -QlpExe .\qlp.exe
#   pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\stabilizer-search.ps1 -N 3 -Stab ZZI IZZ -Top 40 -CommMode facts -CommFacts .\tests\commfacts_pauli_3q_full.txt -QlpExe .\qlp.exe

function QlpComm([string]$p, [string]$q) {
  if ($p.Length -ne $q.Length) { throw "Length mismatch: '$p' vs '$q'" }

  $x = $p
  $y = $q
  if ($x -gt $y) {
    $tmp = $x
    $x = $y
    $y = $tmp
  }

  $sep = [char]31
  $cacheKey = "{0}{6}{1}{6}{2}{6}{3}{6}{4}{6}{5}" -f $script:CommMode, $script:Model, $script:CommFacts, $x, $y, $script:QlpExe, $sep
  if ($script:CommCache.ContainsKey($cacheKey)) { return [bool]$script:CommCache[$cacheKey] }

  $needModel = ($script:CommMode -eq "pauli")
  $needCommFacts = ($script:CommMode -eq "facts")

  if ($needModel -and [string]::IsNullOrWhiteSpace($script:Model)) { throw "CommMode '$($script:CommMode)' requires -Model." }
  if ($needModel -and (-not (Test-Path -LiteralPath $script:Model))) { throw "Model file not found for CommMode '$($script:CommMode)': $($script:Model)" }

  if ($needCommFacts -and [string]::IsNullOrWhiteSpace($script:CommFacts)) { throw "CommMode 'facts' requires -CommFacts." }
  if ($needCommFacts -and (-not (Test-Path -LiteralPath $script:CommFacts))) { throw "Comm facts file not found: $($script:CommFacts)" }

  $qlpArgs = @()
  if ($script:CommMode -eq "pauli") { $qlpArgs += @("--model", $script:Model) }
  if ($script:CommMode -eq "facts") { $qlpArgs += @("--comm-facts", $script:CommFacts) }
  $qlpArgs += @("--comm", $script:CommMode, "comm-check", $x, $y)

  $raw = & $script:QlpExe @qlpArgs 2>&1
  $exitCode = $LASTEXITCODE
  $lines = @($raw | ForEach-Object { "$_" } | Where-Object { $_ -and $_.Trim() })

  if ($exitCode -ne 0) {
    $msg = if ($lines.Count -gt 0) { $lines -join "`n" } else { "(no output)" }
    throw "QLP comm-check failed (exit=$exitCode): $($script:QlpExe) $($qlpArgs -join ' ')`n$msg"
  }
  if ($lines.Count -eq 0) { throw "QLP comm-check returned no output: $($script:QlpExe) $($qlpArgs -join ' ')" }

  $jsonLine = $null
  for ($i = $lines.Count - 1; $i -ge 0; $i--) {
    $t = $lines[$i].Trim()
    if ($t.StartsWith("{") -and $t.EndsWith("}")) { $jsonLine = $t; break }
  }
  if (-not $jsonLine) { $jsonLine = $lines[$lines.Count - 1].Trim() }

  try { $obj = $jsonLine | ConvertFrom-Json } catch { throw "Failed to parse QLP comm-check JSON output: $jsonLine" }
  if ($null -eq $obj -or $null -eq $obj.comm) { throw "QLP comm-check output does not contain field 'comm': $jsonLine" }

  $ans = [bool]$obj.comm
  $script:CommCache[$cacheKey] = $ans
  return $ans
}

function PauliMulChar([char]$a, [char]$b) {
  if ($a -eq 'I') { return $b }
  if ($b -eq 'I') { return $a }
  if ($a -eq $b) { return 'I' }
  $s = ([string]$a + [string]$b)
  if ($s -eq 'XY' -or $s -eq 'YX') { return 'Z' }
  if ($s -eq 'YZ' -or $s -eq 'ZY') { return 'X' }
  if ($s -eq 'ZX' -or $s -eq 'XZ') { return 'Y' }
  throw "Bad Pauli chars: $a $b"
}

function PauliMulWord([string]$p, [string]$q) {
  if ($p.Length -ne $q.Length) { throw "Length mismatch: '$p' vs '$q'" }
  $r = ""
  for ($i=0; $i -lt $p.Length; $i++) { $r += [string](PauliMulChar $p[$i] $q[$i]) }
  return $r
}

function Weight([string]$p) {
  $w = 0
  for ($i=0; $i -lt $p.Length; $i++) { if ($p[$i] -ne 'I') { $w++ } }
  return $w
}

function EnumAllPaulis([int]$n) {
  $alphabet = @('I','X','Y','Z')
  $res = New-Object System.Collections.Generic.List[string]
  function Go([int]$i, [string]$pref) {
    if ($i -eq $n) { $res.Add($pref) | Out-Null; return }
    foreach ($c in $alphabet) { Go ($i+1) ($pref + [string]$c) }
  }
  Go 0 ""
  return ,@($res)
}

# -------- manual CLI parse (so "-Stab ZZI IZZ" works under pwsh -File) --------
$N = 3
$Top = 40
$StabRaw = @()
$script:CommMode = "pauli"
$script:Model = ".\tests\conf_demo_pauli_3q.conf"
$script:CommFacts = $null
$script:QlpExe = "qlp"
$script:CommCache = @{}

for ($i = 0; $i -lt $args.Count; $i++) {
  $a = [string]$args[$i]
  switch ($a) {
    '-N' {
      if ($i + 1 -ge $args.Count) { throw "Missing value after -N" }
      $i++
      $N = [int]$args[$i]
      break
    }
    '-Top' {
      if ($i + 1 -ge $args.Count) { throw "Missing value after -Top" }
      $i++
      $Top = [int]$args[$i]
      break
    }
    '-Stab' {
      if ($i + 1 -ge $args.Count) { throw "Missing value(s) after -Stab" }
      $i++
      while ($i -lt $args.Count) {
        $v = [string]$args[$i]
        if ($v -match '^-') { $i--; break }
        $StabRaw += $v
        $i++
      }
      break
    }
    '-CommMode' {
      if ($i + 1 -ge $args.Count) { throw "Missing value after -CommMode" }
      $i++
      $script:CommMode = [string]$args[$i]
      break
    }
    '-Model' {
      if ($i + 1 -ge $args.Count) { throw "Missing value after -Model" }
      $i++
      $script:Model = [string]$args[$i]
      break
    }
    '-CommFacts' {
      if ($i + 1 -ge $args.Count) { throw "Missing value after -CommFacts" }
      $i++
      $script:CommFacts = [string]$args[$i]
      break
    }
    '-QlpExe' {
      if ($i + 1 -ge $args.Count) { throw "Missing value after -QlpExe" }
      $i++
      $script:QlpExe = [string]$args[$i]
      break
    }
    default {
      throw "Unknown argument: '$a'"
    }
  }
}

if ($script:CommMode -notin @("always","pauli","facts")) { throw "Invalid -CommMode '$($script:CommMode)'. Use always, pauli, or facts." }

# Normalize/flatten: split by commas/whitespace, drop empties
$Stab = @($StabRaw | ForEach-Object { $_ -split '[,\s]+' } | Where-Object { $_ -and $_.Trim() } | ForEach-Object { $_.Trim() })

if ($Stab.Count -eq 0) { throw "Need at least one stabilizer generator. Use: -Stab <g1> <g2> ..." }
foreach ($g in $Stab) { if ($g.Length -ne $N) { throw "Generator length mismatch: '$g' (need N=$N)" } }

$I = ("I" * $N)

# stabilizer group (phase ignored): enumerate products of generators
$r = $Stab.Count
$stabSet = New-Object System.Collections.Generic.HashSet[string]
for ($mask=0; $mask -lt (1 -shl $r); $mask++) {
  $acc = $I
  for ($j=0; $j -lt $r; $j++) {
    if ((($mask -shr $j) -band 1) -eq 1) { $acc = PauliMulWord $acc $Stab[$j] }
  }
  [void]$stabSet.Add($acc)
}

# centralizer: commute with all generators (checked via qlp comm-check)
$all = EnumAllPaulis $N
$cent = @()
foreach ($p in $all) {
  $ok = $true
  foreach ($g in $Stab) {
    if (-not (QlpComm $p $g)) { $ok = $false; break }
  }
  if ($ok) { $cent += $p }
}

$logical = @()
foreach ($p in $cent) { if (-not $stabSet.Contains($p) -and $p -ne $I) { $logical += $p } }

Write-Host ("N={0}, gens={1}" -f $N, ($Stab -join ","))
Write-Host ("|StabGroup| (phase ignored) = {0}" -f $stabSet.Count)
Write-Host ("|Centralizer|               = {0}" -f $cent.Count)
Write-Host ("Logical candidates (cent \ stab, excl I) = {0}" -f $logical.Count)

Write-Host ""
Write-Host "Stabilizer group elements:"
($stabSet | Sort-Object) | ForEach-Object { Write-Host ("  {0}" -f $_) }

Write-Host ""
Write-Host ("Top logical candidates by weight (Top={0}):" -f $Top)
$logical | Sort-Object @{Expression={Weight $_}}, @{Expression={$_}} | Select-Object -First $Top | ForEach-Object { Write-Host ("  w={0}  {1}" -f (Weight $_), $_) }