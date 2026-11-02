#!/usr/bin/env bash
# End-to-end checks of the qlp CLI against the runs reported for the demos.
# Usage: tests/run-cli-tests.sh [path-to-qlp]   (default: qlp on PATH)
set -u
QLP=${1:-qlp}
cd "$(dirname "$0")/.."
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
pass=0; fail=0
expect() { # name, expected substring, actual
  if [[ "$3" == *"$2"* ]]; then pass=$((pass+1)); echo "  ok   $1"
  else fail=$((fail+1)); echo "  FAIL $1"; echo "       expected to contain: $2"; echo "       actual: $3"; fi
}
last() { grep -v '^\s*$' "$1" | tail -n 1; }

Q=tests/demo/qprog_F_demoD_ghz_style_incompat.txt; G=tests/demo/goal_F_demoD_ghz_style_incompat.txt
"$QLP" --comm always compare-jsonl "$TMP/a.jsonl" $Q $G >/dev/null
expect "GHZ-style / always: provable" '"verdict":"provable","sols_hilbert":["{}"],"sols_always":["{}"],"comm_fail_pair":null' "$(last $TMP/a.jsonl)"
"$QLP" --comm pauli --model tests/conf_demo_pauli_3q.conf compare-jsonl "$TMP/p.jsonl" $Q $G >/dev/null
expect "GHZ-style / pauli: not-applicable" '"verdict":"not-applicable","sols_hilbert":[],"sols_always":["{}"],"comm_fail_pair":["XII","ZZI"]' "$(last $TMP/p.jsonl)"
dbg=$("$QLP" --debug --comm pauli --model tests/conf_demo_pauli_3q.conf compare-jsonl "$TMP/p2.jsonl" $Q $G 2>&1 >/dev/null)
expect "GHZ-style / pauli --debug trace" '[comm-fail pos] k=0 pair=Atom "XII" [] / Atom "ZZI" [] ctx=[Atom "ZZI" []]' "$dbg"

"$QLP" --comm facts --comm-facts tests/commfacts_min.txt compare-jsonl "$TMP/f.jsonl" tests/demo/qprog_A1_QR.txt tests/demo/goal_A1_QR.txt >/dev/null
expect "facts / A1_QR: provable" '"verdict":"provable"' "$(last $TMP/f.jsonl)"

"$QLP" --model tests/conf_demo_pauli_3q.conf --comm pauli --max-sol 1 compare-jsonl "$TMP/g.jsonl" tests/demo/qprog_F_demo2_group_3q_v1_auto_groups.txt tests/demo/goal_F_demo2_group_3q_v1_auto_groups.txt >/dev/null
expect "grouping 3q / pauli: provable" '"max_sol":1,"vars":[],"verdict":"provable"' "$(last $TMP/g.jsonl)"

expect "comm-check pauli XII ZZI" '"comm_mode":"pauli","a":"XII","b":"ZZI","comm":false' "$("$QLP" --model tests/conf_demo_pauli_3q.conf --comm pauli comm-check XII ZZI)"
expect "comm-check always XII ZZI" '"comm_mode":"always","a":"XII","b":"ZZI","comm":true' "$("$QLP" --model tests/conf_demo_pauli_3q.conf --comm always comm-check XII ZZI)"

# minimal text-format example: not r(X); p(X). r(a). ?- p(a).
printf 'not r(X); p(X).\nr(a).\n' > "$TMP/q.txt"; printf '?- p(a).\n' > "$TMP/g.txt"
expect "minimal example / always: {}" '{}' "$("$QLP" --comm always solve $TMP/q.txt $TMP/g.txt)"

# BC(II) polarity regression: (ZZI -> XII), XII  does not entail  not ZZI
"$QLP" --comm always compare-jsonl "$TMP/n.jsonl" tests/demo/qprog_test_neg_nonunit.txt tests/demo/goal_test_neg_nonunit.txt >/dev/null
expect "neg non-unit (BC(II)) / always: no-solutions" '"verdict":"no-solutions"' "$(last $TMP/n.jsonl)"

# BC(II) gating: not ZZI; not XII.  XII.  ?- not ZZI.  needs comm(ZZI, XII)
Q2=tests/demo/qprog_test_neg_nonunit_bc2.txt; G2=tests/demo/goal_test_neg_nonunit_bc2.txt
"$QLP" --comm always compare-jsonl "$TMP/b.jsonl" $Q2 $G2 >/dev/null
expect "neg non-unit BC(II) / always: provable" '"verdict":"provable"' "$(last $TMP/b.jsonl)"
"$QLP" --comm pauli --model tests/conf_demo_pauli_3q.conf compare-jsonl "$TMP/b2.jsonl" $Q2 $G2 >/dev/null
expect "neg non-unit BC(II) / pauli: not-applicable" '"verdict":"not-applicable","sols_hilbert":[],"sols_always":["{}"],"comm_fail_pair":["ZZI","XII"]' "$(last $TMP/b2.jsonl)"

# Mermin-Peres 2q: 18 noncommuting pairs among the nine observables
A=(XI IX XX IY YI YY XY YX ZZ); n=0
for ((i=0;i<9;i++)); do for ((j=i+1;j<9;j++)); do
  "$QLP" --model tests/conf_demo_mp_2q.conf --comm pauli comm-check ${A[i]} ${A[j]} | grep -q '"comm":false' && n=$((n+1))
done; done
expect "Mermin-Peres: 18 noncommuting edges" "18" "$n"

# stabilizer ZZI, IZZ: centralizer of size 16 under the facts backend
F=tests/commfacts_pauli_3q_full_noheader.txt; c=0
for a in I X Y Z; do for b in I X Y Z; do for d in I X Y Z; do
  ok=1; for g in ZZI IZZ; do "$QLP" --comm facts --comm-facts $F comm-check $a$b$d $g | grep -q '"comm":true' || ok=0; done
  [ $ok = 1 ] && c=$((c+1))
done; done; done
expect "stabilizer ZZI,IZZ: |Centralizer| = 16" "16" "$c"

echo; echo "$pass/$((pass+fail)) CLI checks passed"
[ $fail -eq 0 ]
