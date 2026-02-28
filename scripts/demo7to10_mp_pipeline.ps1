# demo7〜demo10 まとめ（何をしたか／結果／再現コマンド）
# 前提: リポジトリ直下で PowerShell 実行（例: PS C:\Users\yue\qlp> で .\scripts\ 等が見える状態）
# 目的: Pauli モードで (1) ペア可換性ログ生成→(2) 最大可換コンテキスト抽出→(3) Mermin–Peres 魔方陣再構成→(4) 古典 ±1 大域割当て不可能性を検証

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# -------------------------------
# 共通設定（demo7〜demo10）
# -------------------------------
$Model   = ".\tests\conf_demo_mp_2q.conf"
$DemoDir = ".\tests\demo"
$Case    = "F_demo7_pairs"
$OutJson = ".\out_demo7_pairs.jsonl"
$PairDir = Join-Path $DemoDir ("pairs_{0}_{1}" -f $Case, ([IO.Path]::GetFileNameWithoutExtension($Model)))
$Atoms   = @("XI","IX","XX","IY","YI","YY","XY","YX","ZZ")

Write-Host ("Model   : {0}" -f $Model)
Write-Host ("DemoDir : {0}" -f $DemoDir)
Write-Host ("Case    : {0}" -f $Case)
Write-Host ("PairDir : {0}" -f $PairDir)
Write-Host ("Atoms   : {0} (count={1})" -f ($Atoms -join ", "), $Atoms.Count)

# ============================================================
# demo7: ペア可換性ログ生成 +（可換グラフ彩色による）自動グルーピング + 基本サニティ
# ============================================================
# 何をしたか:
# - scripts/auto-group-ham.ps1 を pauli モードで実行し、全ペア(out_pair_*.jsonl) を生成
# - 非可換エッジ数、彩色グループ(k)を出力、qprog/goal/out_demo7_pairs.jsonl を生成
# 結果（あなたのログ）:
# - NonComm edges: 18
# - Groups (k=3): G1: IX,XI,XX / G2: IY,YI,YY / G3: XY,YX,ZZ
# - out_demo7_pairs.jsonl verdict=provable

Write-Host "`n=== demo7: generate pair logs + auto grouping ==="
pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\auto-group-ham.ps1 `
  -Model $Model `
  -OutJsonl $OutJson `
  -OutDir $DemoDir `
  -Case $Case `
  -CommMode pauli `
  -MaxSol 1 `
  -Atoms ($Atoms -join ",")

# --- demo7 後半: out_pair_*.jsonl を読み、IsComm(a,b) を構築（bool/string/verdict 対応） ---
$comm = @{}
function Key([string]$a, [string]$b) { if ($a -lt $b) { "$a|$b" } else { "$b|$a" } }

function Get-CommValue($o) {
  if ($o.PSObject.Properties.Name -contains "comm") {
    $v = $o.comm
    if ($v -is [bool])   { return $v }
    if ($v -is [string]) { return ($v.Trim().ToLower() -eq "true") }
    return [bool]$v
  }
  if ($o.PSObject.Properties.Name -contains "verdict") {
    if ($o.verdict -eq "provable")       { return $true }
    if ($o.verdict -eq "not-applicable") { return $false }
    return $false
  }
  throw "Pair JSON has neither 'comm' nor 'verdict'."
}

Get-ChildItem -LiteralPath $PairDir -File -Filter "out_pair_*.jsonl" | ForEach-Object {
  $line = (Get-Content -LiteralPath $_.FullName | Where-Object { $_.Trim() } | Select-Object -First 1)
  if ($line) {
    $o = $line | ConvertFrom-Json
    $comm[(Key $o.a $o.b)] = (Get-CommValue $o)
  }
}

function IsComm([string]$a, [string]$b) {
  $k = Key $a $b
  return ($comm.ContainsKey($k) -and $comm[$k])
}

Write-Host ("Pair files : {0}" -f ((Get-ChildItem -LiteralPath $PairDir -File -Filter "out_pair_*.jsonl" | Measure-Object).Count))
Write-Host ("Loaded pairs: {0}" -f $comm.Count)
Write-Host ("Sanity IX|IY should be False: {0}" -f (IsComm "IX" "IY"))
Write-Host ("Sanity XI|IX should be True : {0}" -f (IsComm "XI" "IX"))

# ============================================================
# demo8: 最大可換コンテキスト（最大クリーク）列挙（n=9 なので bitmask 全探索で堅牢に）
# ============================================================
# 何をしたか:
# - demo7 のペア表(IsComm)から「可換グラフ」を作り、最大クリーク（極大可換集合）を列挙
# - MP 魔方陣の 2量子ビット版では最大サイズが 3（＝各コンテキストは3つの測定量）
# 結果（あなたのログ）:
# - Contexts (maximal, size=3): 6
#   IX,XI,XX / IX,YI,YX / IY,XI,XY / IY,YI,YY / XX,YY,ZZ / XY,YX,ZZ
# - 出力: .\tests\demo\contexts_demo8_mp_maximal.tsv

Write-Host "`n=== demo8: enumerate maximal commuting contexts (maximal cliques) ==="

function PopCount([int]$x) {
  $c = 0
  while ($x -ne 0) { $x = $x -band ($x - 1); $c++ }
  return $c
}

function IsClique([string[]]$S) {
  for ($i=0; $i -lt $S.Length; $i++) {
    for ($j=$i+1; $j -lt $S.Length; $j++) {
      if (-not (IsComm $S[$i] $S[$j])) { return $false }
    }
  }
  return $true
}

# 全クリーク（サイズ>=3）を集める
$cliques = New-Object System.Collections.Generic.List[object]
for ($mask=0; $mask -lt (1 -shl $Atoms.Count); $mask++) {
  if ((PopCount $mask) -lt 3) { continue }
  $S = @()
  for ($i=0; $i -lt $Atoms.Count; $i++) {
    if ((($mask -shr $i) -band 1) -eq 1) { $S += $Atoms[$i] }
  }
  if (IsClique $S) { $cliques.Add(@($S | Sort-Object)) | Out-Null }
}

# 極大性フィルタ（どの上位集合にも含まれない）
$maxCliques = New-Object System.Collections.Generic.List[object]
for ($i=0; $i -lt $cliques.Count; $i++) {
  $A = $cliques[$i]
  $isMax = $true
  for ($j=0; $j -lt $cliques.Count; $j++) {
    if ($i -eq $j) { continue }
    $B = $cliques[$j]
    if ($B.Count -le $A.Count) { continue }
    $ok = $true
    foreach ($x in $A) { if (-not (@($B) -contains $x)) { $ok = $false; break } }
    if ($ok) { $isMax = $false; break }
  }
  if ($isMax) { $maxCliques.Add($A) | Out-Null }
}

# size==3（MP のコンテキスト）だけを抽出して重複排除
$ctx = $maxCliques | Where-Object { $_.Count -eq 3 } | Sort-Object { $_ -join "," } -Unique
Write-Host ("Contexts (maximal, size=3): {0}" -f $ctx.Count)
$ctx | ForEach-Object { Write-Host ("  {0}" -f (($_ -join ", "))) }

$ctxPath = Join-Path $DemoDir "contexts_demo8_mp_maximal.tsv"
$ctxLines = $ctx | ForEach-Object { $_ -join "`t" }
Set-Content -Encoding utf8NoBOM -LiteralPath $ctxPath -Value (($ctxLines -join "`n") + "`n")
Write-Host ("Wrote: {0}" -f $ctxPath)

# ============================================================
# demo9: 6つの最大コンテキストから MP 魔方陣(3x3)を再構成し、行列積の符号を検証
# ============================================================
# 何をしたか:
# - contexts_demo8_mp_maximal.tsv を読み込み、3行+3列に分割できる組を探索
# - 行×列の交点（共有原子）で 3x3 を復元
# - Pauli 乗算で各行/列の積を計算し、最後の列だけ -I になることを確認
# 結果（あなたのログ）:
# - Row1: XI, IX, XX  / Row2: IY, YI, YY / Row3: XY, YX, ZZ
# - Col1: XI, IY, XY / Col2: IX, YI, YX / Col3: XX, YY, ZZ
# - Products: Row1=+I, Row2=+I, Row3=+I, Col1=+I, Col2=+I, Col3=-I
# - 出力: .\tests\demo\square_demo9_mp.tsv

Write-Host "`n=== demo9: reconstruct MP square + verify row/col products ==="

# load contexts
$Ctx = New-Object System.Collections.Generic.List[object]
Get-Content -LiteralPath $ctxPath | Where-Object { $_.Trim() } | ForEach-Object {
  $xs = $_.Trim() -split "`t"
  if ($xs.Length -ne 3) { throw "Bad line (need 3 fields): $_" }
  $Ctx.Add(@([string]$xs[0],[string]$xs[1],[string]$xs[2])) | Out-Null
}
Write-Host ("Loaded contexts: {0}" -f $Ctx.Count)

function IntersectElems($A, $B) {
  $setB = New-Object System.Collections.Generic.HashSet[string]
  foreach ($y in @($B)) { [void]$setB.Add([string]$y) }
  $res = @()
  foreach ($x in @($A)) { if ($setB.Contains([string]$x)) { $res += [string]$x } }
  return ,$res
}
function IntersectCount($A, $B) { return (IntersectElems $A $B).Length }
function Disjoint($A, $B) { return ((IntersectCount $A $B) -eq 0) }
function Inter1($A, $B) {
  $xs = IntersectElems $A $B
  if ($xs.Length -ne 1) { return $null }
  return [string]$xs[0]
}
function UnionSize3($c1, $c2, $c3) {
  $s = New-Object System.Collections.Generic.HashSet[string]
  foreach ($x in @($c1)) { [void]$s.Add([string]$x) }
  foreach ($x in @($c2)) { [void]$s.Add([string]$x) }
  foreach ($x in @($c3)) { [void]$s.Add([string]$x) }
  return $s.Count
}

# unique atoms
$All = @()
foreach ($c in $Ctx) { $All += @($c) }
$AtomsU = @($All | Sort-Object -Unique)
$U = $AtomsU.Length
Write-Host ("Unique atoms: {0}  [{1}]" -f $U, ($AtomsU -join ", "))

# find partition into 3 rows + 3 cols (each triple disjoint; each row meets each col in exactly 1 atom)
$Rows = $null
$Cols = $null
$idx = 0..($Ctx.Count-1)

for ($a=0; $a -lt $idx.Length; $a++) {
  for ($b=$a+1; $b -lt $idx.Length; $b++) {
    for ($c=$b+1; $c -lt $idx.Length; $c++) {
      $r1 = $Ctx[$idx[$a]]; $r2 = $Ctx[$idx[$b]]; $r3 = $Ctx[$idx[$c]]
      if (-not (Disjoint $r1 $r2 -and Disjoint $r1 $r3 -and Disjoint $r2 $r3)) { continue }
      if ((UnionSize3 $r1 $r2 $r3) -ne $U) { continue }

      $rest = @()
      foreach ($t in $idx) { if ($t -ne $idx[$a] -and $t -ne $idx[$b] -and $t -ne $idx[$c]) { $rest += $t } }
      $s1 = $Ctx[$rest[0]]; $s2 = $Ctx[$rest[1]]; $s3 = $Ctx[$rest[2]]

      if (-not (Disjoint $s1 $s2 -and Disjoint $s1 $s3 -and Disjoint $s2 $s3)) { continue }
      if ((UnionSize3 $s1 $s2 $s3) -ne $U) { continue }

      $okX = $true
      foreach ($ri in @($r1,$r2,$r3)) {
        foreach ($sj in @($s1,$s2,$s3)) {
          if ((IntersectCount $ri $sj) -ne 1) { $okX = $false; break }
        }
        if (-not $okX) { break }
      }
      if (-not $okX) { continue }

      $Rows = @($r1,$r2,$r3)
      $Cols = @($s1,$s2,$s3)
      break
    }
    if ($Rows) { break }
  }
  if ($Rows) { break }
}
if (-not $Rows) { throw "No valid (rows, cols) partition found." }

function HasBoth($ctx, [string]$a, [string]$b) { (@($ctx) -contains $a) -and (@($ctx) -contains $b) }

# Ensure Rows contains the "XI,IX,XX" line (otherwise swap)
if (-not ($Rows | Where-Object { HasBoth $_ "XI" "IX" } | Select-Object -First 1)) {
  $tmp = $Rows; $Rows = $Cols; $Cols = $tmp
}

# canonical ordering by MP atoms
$Row1 = $Rows | Where-Object { HasBoth $_ "XI" "IX" } | Select-Object -First 1
$Row2 = $Rows | Where-Object { HasBoth $_ "IY" "YI" } | Select-Object -First 1
$Row3 = $Rows | Where-Object { HasBoth $_ "XY" "YX" } | Select-Object -First 1
$Col1 = $Cols | Where-Object { HasBoth $_ "XI" "IY" } | Select-Object -First 1
$Col2 = $Cols | Where-Object { HasBoth $_ "IX" "YI" } | Select-Object -First 1
$Col3 = $Cols | Where-Object { HasBoth $_ "XX" "YY" } | Select-Object -First 1
if (-not ($Row1 -and $Row2 -and $Row3 -and $Col1 -and $Col2 -and $Col3)) { throw "Canonical labeling failed." }

$R = @($Row1,$Row2,$Row3)
$C = @($Col1,$Col2,$Col3)

# build 3x3 square by intersections
$M = @()
for ($i=0; $i -lt 3; $i++) {
  $row = @()
  for ($j=0; $j -lt 3; $j++) {
    $x = Inter1 $R[$i] $C[$j]
    if (-not $x) { throw "Bad intersection at ($i,$j)." }
    $row += $x
  }
  $M += ,$row
}

Write-Host "Reconstructed square:"
Write-Host ("  Row1: {0}" -f ($M[0] -join ", "))
Write-Host ("  Row2: {0}" -f ($M[1] -join ", "))
Write-Host ("  Row3: {0}" -f ($M[2] -join ", "))
Write-Host ("  Col1: {0}" -f (@($M[0][0],$M[1][0],$M[2][0]) -join ", "))
Write-Host ("  Col2: {0}" -f (@($M[0][1],$M[1][1],$M[2][1]) -join ", "))
Write-Host ("  Col3: {0}" -f (@($M[0][2],$M[1][2],$M[2][2]) -join ", "))

# Pauli multiplication (2-qubit), track phase i^k (k mod 4)
function PhaseStr([int]$k) { switch ($k % 4) { 0 {"+1"} 1 {"+i"} 2 {"-1"} 3 {"-i"} } }
function Mul1([char]$a, [char]$b) {
  if ($a -eq 'I') { return @{k=0; o=$b} }
  if ($b -eq 'I') { return @{k=0; o=$a} }
  if ($a -eq $b)  { return @{k=0; o='I'} }
  switch ("$a$b") {
    "XY" { @{k=1; o='Z'} }  "YX" { @{k=3; o='Z'} }
    "YZ" { @{k=1; o='X'} }  "ZY" { @{k=3; o='X'} }
    "ZX" { @{k=1; o='Y'} }  "XZ" { @{k=3; o='Y'} }
    default { throw "bad pair: $a $b" }
  }
}
function Mul2([string]$A, [string]$B) {
  $m1 = Mul1 $A[0] $B[0]
  $m2 = Mul1 $A[1] $B[1]
  @{ k = (($m1.k + $m2.k) % 4); o = ("" + $m1.o + $m2.o) }
}
function Prod([string[]]$ops) {
  $k=0; $o="II"
  foreach ($x in @($ops)) {
    $m = Mul2 $o $x
    $k = ($k + $m.k) % 4
    $o = $m.o
  }
  "{0} {1}" -f (PhaseStr $k), $o
}

$RowOps = @($M[0], $M[1], $M[2])
$ColOps = @(
  @($M[0][0],$M[1][0],$M[2][0]),
  @($M[0][1],$M[1][1],$M[2][1]),
  @($M[0][2],$M[1][2],$M[2][2])
)

Write-Host "Products:"
Write-Host ("  Row1: {0}" -f (Prod $RowOps[0]))
Write-Host ("  Row2: {0}" -f (Prod $RowOps[1]))
Write-Host ("  Row3: {0}" -f (Prod $RowOps[2]))
Write-Host ("  Col1: {0}" -f (Prod $ColOps[0]))
Write-Host ("  Col2: {0}" -f (Prod $ColOps[1]))
Write-Host ("  Col3: {0}" -f (Prod $ColOps[2]))

# write square
$sqPath = Join-Path $DemoDir "square_demo9_mp.tsv"
$sqLines = @(
  ($M[0] -join "`t"),
  ($M[1] -join "`t"),
  ($M[2] -join "`t")
)
Set-Content -Encoding utf8NoBOM -LiteralPath $sqPath -Value (($sqLines -join "`n") + "`n")
Write-Host ("Wrote: {0}" -f $sqPath)

# ============================================================
# demo10: MP 制約に対する「古典的 ±1 大域割当」が存在しないことを総当たりで検証
# ============================================================
# 何をしたか:
# - square_demo9_mp.tsv から 6コンテキスト（Row1..Row3, Col1..Col3）を取り出し、要求符号を設定
#   Row1..Row3=+1, Col1=+1, Col2=+1, Col3=-1
# - 9変数(±1)^9 の全割当てを総当たりして、全6制約を同時に満たすものが 0 であることを確認
# - さらに部分集合 satisfiable を全探索し、「6本全部」だけが UNSAT であることも確認（最小 UNSAT core が全体）
# 結果（あなたのログ）:
# - Satisfying assignments: 0
# - Max satisfied constraints (out of 6): 5
# - subset summary: size 1..5 は全部 SAT, size 6 は UNSAT のみ

Write-Host "`n=== demo10: global ±1 assignment check (Kochen–Specker style) ==="

# load 3x3 square
$M2 = @()
Get-Content -LiteralPath $sqPath | Where-Object { $_.Trim() } | ForEach-Object {
  $xs = $_.Trim() -split "`t"
  if ($xs.Length -ne 3) { throw "Bad line (need 3 fields): $_" }
  $M2 += ,@([string]$xs[0],[string]$xs[1],[string]$xs[2])
}
if ($M2.Count -ne 3) { throw "Need 3 rows in square file." }

Write-Host "Loaded square:"
Write-Host ("  Row1: {0}" -f ($M2[0] -join ", "))
Write-Host ("  Row2: {0}" -f ($M2[1] -join ", "))
Write-Host ("  Row3: {0}" -f ($M2[2] -join ", "))
Write-Host ("  Col1: {0}" -f (@($M2[0][0],$M2[1][0],$M2[2][0]) -join ", "))
Write-Host ("  Col2: {0}" -f (@($M2[0][1],$M2[1][1],$M2[2][1]) -join ", "))
Write-Host ("  Col3: {0}" -f (@($M2[0][2],$M2[1][2],$M2[2][2]) -join ", "))

# unique atoms
$All2 = @($M2[0] + $M2[1] + $M2[2])
$AtomsU2 = @($All2 | Sort-Object -Unique)
if ($AtomsU2.Length -ne 9) { throw ("Expected 9 unique atoms, got {0}: {1}" -f $AtomsU2.Length, ($AtomsU2 -join ", ")) }
Write-Host ("Unique atoms: {0}  [{1}]" -f $AtomsU2.Length, ($AtomsU2 -join ", "))

# contexts with required product sign (±1)
$Ctxs = @(
  @{ name="Row1"; ops=@($M2[0][0],$M2[0][1],$M2[0][2]); sign=+1 },
  @{ name="Row2"; ops=@($M2[1][0],$M2[1][1],$M2[1][2]); sign=+1 },
  @{ name="Row3"; ops=@($M2[2][0],$M2[2][1],$M2[2][2]); sign=+1 },
  @{ name="Col1"; ops=@($M2[0][0],$M2[1][0],$M2[2][0]); sign=+1 },
  @{ name="Col2"; ops=@($M2[0][1],$M2[1][1],$M2[2][1]); sign=+1 },
  @{ name="Col3"; ops=@($M2[0][2],$M2[1][2],$M2[2][2]); sign=-1 }
)

function ProdVal($assign, [string[]]$ops) {
  $p = 1
  foreach ($x in @($ops)) { $p *= [int]$assign[[string]$x] }
  return $p
}
function CheckMask($assign, [int]$mask) {
  for ($i=0; $i -lt 6; $i++) {
    if ((($mask -shr $i) -band 1) -eq 0) { continue }
    $c = $Ctxs[$i]
    if ((ProdVal $assign $c.ops) -ne $c.sign) { return $false }
  }
  return $true
}
function CheckAll($assign) { return (CheckMask $assign ((1 -shl 6) - 1)) }

# brute force 2^9 assignments + record best
$sol = @()
$best = -1
$bestEx = $null
for ($mask=0; $mask -lt (1 -shl 9); $mask++) {
  $assign = @{}
  for ($i=0; $i -lt 9; $i++) { $assign[$AtomsU2[$i]] = ((($mask -shr $i) -band 1) -eq 1) ? 1 : -1 }

  $okCount = 0
  foreach ($c in $Ctxs) { if ((ProdVal $assign $c.ops) -eq $c.sign) { $okCount++ } }
  if ($okCount -gt $best) { $best = $okCount; $bestEx = $assign }

  if (CheckAll $assign) { $sol += ,$assign }
}
Write-Host ("Satisfying assignments: {0}" -f $sol.Count)
Write-Host ("Max satisfied constraints (out of 6): {0}" -f $best)

if ($bestEx) {
  Write-Host "Example assignment achieving the max:"
  foreach ($a in $AtomsU2) { Write-Host ("  v({0}) = {1}" -f $a, $bestEx[$a]) }
  Write-Host "Constraint check for that example:"
  foreach ($c in $Ctxs) { Write-Host ("  {0}: prod={1}  need={2}  ops={3}" -f $c.name, (ProdVal $bestEx $c.ops), $c.sign, ($c.ops -join ", ")) }
}

# subset satisfiability (all subsets of 6 constraints)
function FindWitness([int]$mask6) {
  for ($m=0; $m -lt (1 -shl 9); $m++) {
    $a = @{}
    for ($i=0; $i -lt 9; $i++) { $a[$AtomsU2[$i]] = ((($m -shr $i) -band 1) -eq 1) ? 1 : -1 }
    if (CheckMask $a $mask6) { return $a }
  }
  return $null
}

$bySizeSat   = @{ 1=0; 2=0; 3=0; 4=0; 5=0; 6=0 }
$bySizeUnsat = @{ 1=0; 2=0; 3=0; 4=0; 5=0; 6=0 }
$UnsatMasks  = @()

for ($mask6=1; $mask6 -lt (1 -shl 6); $mask6++) {
  $sz = PopCount $mask6
  $w = FindWitness $mask6
  if ($w) { $bySizeSat[$sz]++ } else { $bySizeUnsat[$sz]++; $UnsatMasks += $mask6 }
}

Write-Host "Subset satisfiability summary (size -> sat/unsat):"
for ($s=1; $s -le 6; $s++) { Write-Host ("  {0}: sat={1}  unsat={2}" -f $s, $bySizeSat[$s], $bySizeUnsat[$s]) }

# minimal UNSAT cores
$MinCores = @()
foreach ($mask6 in $UnsatMasks) {
  $isMin = $true
  for ($i=0; $i -lt 6; $i++) {
    if ((($mask6 -shr $i) -band 1) -eq 0) { continue }
    $sub = $mask6 -band (-bnot (1 -shl $i))
    if ($sub -eq 0) { continue }
    if (-not (FindWitness $sub)) { $isMin = $false; break }
  }
  if ($isMin) { $MinCores += $mask6 }
}

Write-Host ("Minimal UNSAT cores: {0}" -f $MinCores.Count)
$k = 1
foreach ($mask6 in $MinCores) {
  $names = @()
  for ($i=0; $i -lt 6; $i++) { if ((($mask6 -shr $i) -band 1) -eq 1) { $names += $Ctxs[$i].name } }
  Write-Host ("  Core{0}: {1}" -f $k, ($names -join ", "))
  $k++
}

Write-Host "Witness for each 5-constraint set (drop exactly one):"
for ($drop=0; $drop -lt 6; $drop++) {
  $mask5 = ((1 -shl 6) - 1) -band (-bnot (1 -shl $drop))
  $w = FindWitness $mask5
  Write-Host ("  drop {0}: {1}" -f $Ctxs[$drop].name, ($w ? "SAT" : "UNSAT"))
  if ($w) {
    $line = ($AtomsU2 | ForEach-Object { "{0}={1}" -f $_, $w[$_] }) -join " "
    Write-Host ("    {0}" -f $line)
  }
}

# write solutions (should be empty)
$solPath = Join-Path $DemoDir "solutions_demo10_mp.txt"
$lines = @()
foreach ($s in $sol) { $lines += ($AtomsU2 | ForEach-Object { "{0}={1}" -f $_, $s[$_] }) -join " " }
Set-Content -Encoding utf8NoBOM -LiteralPath $solPath -Value (($lines -join "`n") + "`n")
Write-Host ("Wrote: {0}" -f $solPath)

Write-Host "`nDONE: demo7〜demo10 pipeline complete (pair logs → maximal contexts → MP square → classical UNSAT)."
