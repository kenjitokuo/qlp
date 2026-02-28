param(
  [Parameter(Mandatory=$true)][string]$InOps,
  [ValidateSet("Lex","ZFirst")][string]$Mode = "ZFirst"
)

function MustReadLines([string]$path) {
  if (-not (Test-Path -LiteralPath $path)) { throw "Missing file: $path" }
  $xs = Get-Content -LiteralPath $path | ForEach-Object { $_.Trim() } | Where-Object { $_ }
  return ,@($xs)
}

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

function KeyLex([string]$p) {
  $map = @{ 'I'='0'; 'X'='1'; 'Y'='2'; 'Z'='3' }
  $k = ""
  for ($i=0; $i -lt $p.Length; $i++) { $k += $map[[string]$p[$i]] }
  return $k
}

function KeyZFirst([string]$p) {
  $z = 0; $w = 0
  for ($i=0; $i -lt $p.Length; $i++) {
    $c = $p[$i]
    if ($c -eq 'Z') { $z++ }
    if ($c -ne 'I') { $w++ }
  }
  return ("{0:D2}:{1:D2}:{2}" -f (99-$z), $w, (KeyLex $p))
}

$ops = MustReadLines $InOps
if ($ops.Count -le 1) { Write-Host "Trivial (<=1 op)."; exit 0 }

$target = @($ops)
if ($Mode -eq "Lex") { $target = @($target | Sort-Object { KeyLex $_ }) }
else { $target = @($target | Sort-Object { KeyZFirst $_ }) }

$cur = @($ops)
$blocked = $null

for ($pass=0; $pass -lt $cur.Count; $pass++) {
  $moved = $false
  for ($i=0; $i -lt ($cur.Count-1); $i++) {
    if ($cur[$i] -eq $target[$i]) { continue }
    $a = $cur[$i]; $b = $cur[$i+1]
    $ka = ($Mode -eq "Lex") ? (KeyLex $a) : (KeyZFirst $a)
    $kb = ($Mode -eq "Lex") ? (KeyLex $b) : (KeyZFirst $b)
    if ($ka -le $kb) { continue }
    if (PauliComm $a $b) {
      $cur[$i] = $b; $cur[$i+1] = $a
      $moved = $true
    } else {
      $blocked = @($a,$b)
      break
    }
  }
  if ($blocked) { break }
  if (-not $moved) { break }
}

Write-Host ("Mode   : {0}" -f $Mode)
Write-Host ("Input  : {0}" -f ($ops -join " "))
Write-Host ("Target : {0}" -f ($target -join " "))
Write-Host ("Result : {0}" -f ($cur -join " "))

if ($blocked) {
  Write-Host ("FAILED (blocked by noncommuting adjacent pair): {0} / {1}" -f $blocked[0], $blocked[1])
  exit 2
} else {
  $ok = $true
  for ($i=0; $i -lt $cur.Count; $i++) { if ($cur[$i] -ne $target[$i]) { $ok = $false; break } }
  if ($ok) { Write-Host "OK (rewritten to target)"; exit 0 }
  else { Write-Host "STOP (no more commuting swaps, but not at target)"; exit 1 }
}
