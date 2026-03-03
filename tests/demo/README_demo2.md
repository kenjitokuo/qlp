# Demo2: Hamiltonian term grouping by commutativity

This demo groups Hamiltonian terms into commuting sets (useful for measurement grouping / Trotterization).

## Run

```powershell
pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\run-demo2.ps1 -Model .\tests\conf_demo_ham_2q.conf -Case B_auto4 -CommMode pauli -Atoms Q,R,S,T
```

## Inputs

* Model: `.\tests\conf_demo_ham_2q.conf`
* CommMode: `pauli`
* Atoms: `Q, R, S, T`

## Generated files

* QProgram: `.\tests\demo\qprog_B_auto4_auto_groups.txt`
* Goal: `.\tests\demo\goal_B_auto4_auto_groups.txt`
* OutJsonl: `.\out_demo_ham_auto.jsonl`

## Pair commutativity (from `qlp comm-check`)

```json
{"model":".\\tests\\conf_demo_ham_2q.conf","comm_mode":"pauli","a":"Q","b":"R","comm":true}
{"model":".\\tests\\conf_demo_ham_2q.conf","comm_mode":"pauli","a":"Q","b":"S","comm":false}
{"model":".\\tests\\conf_demo_ham_2q.conf","comm_mode":"pauli","a":"Q","b":"T","comm":false}
{"model":".\\tests\\conf_demo_ham_2q.conf","comm_mode":"pauli","a":"R","b":"S","comm":false}
{"model":".\\tests\\conf_demo_ham_2q.conf","comm_mode":"pauli","a":"R","b":"T","comm":false}
{"model":".\\tests\\conf_demo_ham_2q.conf","comm_mode":"pauli","a":"S","b":"T","comm":true}
```

## Final verdict (last JSONL line)

```json
{"model":".\\tests\\conf_demo_ham_2q.conf","qprog":".\\tests\\demo\\qprog_B_auto4_auto_groups.txt","goal":".\\tests\\demo\\goal_B_auto4_auto_groups.txt","max_sol":20,"vars":[],"verdict":"provable","sols_hilbert":["{}"],"sols_always":["{}"],"comm_fail_pair":null,"case":"B_auto4","model_tag":"conf_demo_ham_2q","comm_mode":"pauli","n_atoms":4,"atoms":["Q","R","S","T"],"noncomm_edge_count":4,"noncomm_edges":["Q|S","Q|T","R|S","R|T"],"k_opt":2,"group_sizes":[2,2],"k_greedy":2,"greedy_group_sizes":[2,2]}
```

## Goal (for reference)

```text
Goal {wantPos = [Atom "G1" [TFun "a" []], Atom "G2" [TFun "a" []]], wantNeg = []}
```

