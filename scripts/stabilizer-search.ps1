# stabilizer-search.ps1
# Usage example (your current style works):
#   pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\stabilizer-search.ps1 -N 3 -Stab ZZI IZZ -Top 40

function PauliComm([string]$p, [string]$q) {
  if ($p.Length -ne $q.Length) { throw "Length mismatch: '$p' vs '$q'" }
  $anti = 0
  for ($i=0; $i -lt $p.Length; $i++) {
    $a = $p[$i]; $b = $q[$i]
    if ($a -eq 'I' -or $b -eq 'I') { continue }
    if ($a -eq $b) { continue }
    $anti++
  }
  return (($anti % 2) -eq 0)
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
        if ($v -match '^-') { $i--; break }  # next option begins; let outer loop handle it
        $StabRaw += $v
        $i++
      }
      break
    }
    default {
      throw "Unknown argument: '$a'"
    }
  }
}

# Normalize/flatten: split by commas/whitespace, drop empties
$Stab = @(
  $StabRaw |
    ForEach-Object { $_ -split '[,\s]+' } |
    Where-Object { $_ -and $_.Trim() } |
    ForEach-Object { $_.Trim() }
)

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

# centralizer: commute with all generators
$all = EnumAllPaulis $N
$cent = @()
foreach ($p in $all) {
  $ok = $true
  foreach ($g in $Stab) { if (-not (PauliComm $p $g)) { $ok = $false; break } }
  if ($ok) { $cent += $p }
}

$logical = @()
foreach ($p in $cent) { if (-not $stabSet.Contains($p) -and $p -ne $I) { $logical += $p } }

Write-Host ("N={0}, gens={1}" -f $N, ($Stab -join ","))
Write-Host ("|StabGroup| (phase ignored) = {0}" -f $stabSet.Count)
Write-Host ("|Centralizer|               = {0}" -f $cent.Count)
Write-Host ("Logical candidates (cent \\ stab, excl I) = {0}" -f $logical.Count)

Write-Host ""
Write-Host "Stabilizer group elements:"
($stabSet | Sort-Object) | ForEach-Object { Write-Host ("  {0}" -f $_) }

Write-Host ""
Write-Host ("Top logical candidates by weight (Top={0}):" -f $Top)
$logical | Sort-Object @{Expression={Weight $_}}, @{Expression={$_}} | Select-Object -First $Top | ForEach-Object {
  Write-Host ("  w={0}  {1}" -f (Weight $_), $_)
}
