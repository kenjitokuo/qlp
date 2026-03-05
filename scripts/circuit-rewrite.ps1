param(
  [Parameter(Mandatory=$true)][string]$InOps,
  [ValidateSet("Lex","ZFirst")][string]$Mode = "ZFirst",
  [ValidateSet("always","pauli","facts")][string]$CommMode = "pauli",
  [string]$Model = ".\tests\conf_demo_pauli_3q.conf",
  [string]$QlpExe = "qlp"
)

function MustReadLines([string]$path) {
  if (-not (Test-Path -LiteralPath $path)) { throw "Missing file: $path" }
  $xs = Get-Content -LiteralPath $path | ForEach-Object { $_.Trim() } | Where-Object { $_ }
  return ,@($xs)
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

$script:CommCache = @{}

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
  $cacheKey = "{0}{4}{1}{4}{2}{4}{3}" -f $CommMode, $Model, $x, $y, $sep
  if ($script:CommCache.ContainsKey($cacheKey)) {
    return [bool]$script:CommCache[$cacheKey]
  }

  $needModel = ($CommMode -ne "always")
  if ($needModel -and [string]::IsNullOrWhiteSpace($Model)) {
    throw "CommMode '$CommMode' requires -Model."
  }
  if ($needModel -and (-not (Test-Path -LiteralPath $Model))) {
    throw "Model file not found for CommMode '$CommMode': $Model"
  }

  $qlpArgs = @()
  if (-not [string]::IsNullOrWhiteSpace($Model) -and (Test-Path -LiteralPath $Model)) {
    $qlpArgs += @("--model", $Model)
  }
  $qlpArgs += @("--comm", $CommMode, "comm-check", $x, $y)

  $raw = & $QlpExe @qlpArgs 2>&1
  $exitCode = $LASTEXITCODE
  $lines = @($raw | ForEach-Object { "$_" } | Where-Object { $_ -and $_.Trim() })

  if ($exitCode -ne 0) {
    $msg = if ($lines.Count -gt 0) { $lines -join "`n" } else { "(no output)" }
    throw "QLP comm-check failed (exit=$exitCode): $QlpExe $($qlpArgs -join ' ')`n$msg"
  }

  if ($lines.Count -eq 0) {
    throw "QLP comm-check returned no output: $QlpExe $($qlpArgs -join ' ')"
  }

  $jsonLine = $null
  for ($i = $lines.Count - 1; $i -ge 0; $i--) {
    $t = $lines[$i].Trim()
    if ($t.StartsWith("{") -and $t.EndsWith("}")) {
      $jsonLine = $t
      break
    }
  }
  if (-not $jsonLine) {
    $jsonLine = $lines[$lines.Count - 1].Trim()
  }

  try {
    $obj = $jsonLine | ConvertFrom-Json
  } catch {
    throw "Failed to parse QLP comm-check JSON output: $jsonLine"
  }

  if ($null -eq $obj -or $null -eq $obj.comm) {
    throw "QLP comm-check output does not contain field 'comm': $jsonLine"
  }

  $ans = [bool]$obj.comm
  $script:CommCache[$cacheKey] = $ans
  return $ans
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
    if (QlpComm $a $b) {
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
  for ($i=0; $i -lt $cur.Count; $i++) {
    if ($cur[$i] -ne $target[$i]) { $ok = $false; break }
  }
  if ($ok) { Write-Host "OK (rewritten to target)"; exit 0 }
  else { Write-Host "STOP (no more commuting swaps, but not at target)"; exit 1 }
}