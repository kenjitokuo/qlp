# qlp
## Hilbert backend (finite-dimensional commutativity check)

This project includes a finite-dimensional backend to check commutativity and use it as the condition C(·,·) in Quantum Logic Programming style proof search.

Quick start (default):
    stack run

Specify a model file:
    stack run -- --model .\hilbert.conf

If the file is missing, the program falls back to an internal default model.


### Model file format (minimal)

Example:
    dim 2
    eps 1e-9
    pred P Z0
    pred Q Z1
    pred R X+
    pred S X-

Notes:
- dim is currently 2 only (preset projections).
- eps is the threshold for "commutes" (commutator norm <= eps).
- pred <Name> <Preset> assigns a predicate symbol to a preset projection.


--------------------------------------------------------------------------------
## Demo 1: comm_fail_pair isolation (minimal diagnosis set)

Goal
- Separate "not-applicable" caused by commutativity pre-check failure.
- Identify which single non-commuting pair is responsible, by preparing minimal cases.

Cases (each case requires commutativity of exactly one pair)
- A1_QR: requires Q commutes with R
- C2_var_QS: requires Q commutes with S
- A4_RS: requires R commutes with S

Models (non-commuting pairs included; obtained by comm-graph.tsv)
- confArgA: Q||R
- conf_demo_mixed: Q||R, Q||S
- conf_demo_only_qr_noncomm: Q||R
- conf_demo_only_qs_noncomm: Q||S
- conf_demo_only_rs_noncomm: R||S

Observed signal (pair shown in comm_fail_pair)
- If A1_QR becomes not-applicable, the failing pair is Q||R.
- If C2_var_QS becomes not-applicable, the failing pair is Q||S.
- If A4_RS becomes not-applicable, the failing pair is R||S.

How to run (batch)
    Remove-Item .\out_demo.jsonl -ErrorAction SilentlyContinue
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\run-demo-matrix.ps1 -Out ".\out_demo.jsonl" -MaxSol 5
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\inspect-fails.ps1 -In ".\out_demo.jsonl" -Top 50
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\comm-graph.ps1 -In ".\out_demo.jsonl" -Out ".\comm_graph.tsv"
    Get-Content .\comm_graph.tsv

Targeted reproduction (run one case directly)
    stack exec qlp -- --debug --model '.\tests\demo_models\conf_demo_only_qr_noncomm.conf' --comm hilbert --solve '.\tests\demo\qprog_A1_QR.txt' '.\tests\demo\goal_A1_QR.txt'
    stack exec qlp -- --debug --model '.\tests\demo_models\conf_demo_only_qs_noncomm.conf' --comm hilbert --solve '.\tests\demo\qprog_C2_var_QS.txt' '.\tests\demo\goal_C2_var_QS.txt'
    stack exec qlp -- --debug --model '.\tests\demo_models\conf_demo_only_rs_noncomm.conf' --comm hilbert --solve '.\tests\demo\qprog_A4_RS.txt' '.\tests\demo\goal_A4_RS.txt'


--------------------------------------------------------------------------------
## Demo 2: Hamiltonian term grouping by commutativity (Pauli backend)

Motivation
Given H = Σ H_i, group terms into commuting sets so that each group can be measured/handled together.
This is directly relevant to measurement grouping and (conceptually) to Trotter-style decompositions.

We use a 2-qubit Pauli commutativity checker (CommMode: pauli) and atoms that correspond to Pauli strings.

Atom meanings in the default demo model
- Q = Z⊗Z
- R = X⊗X
- S = Z⊗X
- T = X⊗Z

Expected commutativity structure (pairwise)
- Q commutes with R
- S commutes with T
- The cross pairs (Q with S/T, R with S/T) do not commute

So the optimal partition is
- Group 1: {Q, R}
- Group 2: {S, T}


### Demo 2a: auto grouping (should be provable)

Artifacts
- Model: .\tests\conf_demo_ham_2q.conf
- QProgram: .\tests\demo\qprog_B_auto4_auto_groups.txt
- Goal: .\tests\demo\goal_B_auto4_auto_groups.txt
- Out: .\out_demo_ham_auto.jsonl

Command (one shot)
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\run-demo2.ps1

Or (explicit parameters)
    pwsh -NoProfile -ExecutionPolicy Bypass -File .\scripts\run-demo2.ps1 -Model .\tests\conf_demo_ham_2q.conf -Case B_auto4 -CommMode pauli -Atoms Q,R,S,T

Expected result (last line in Out)
- verdict should be provable


### Demo 2b: one-group negative control (should be not-applicable under comm=pauli)

Purpose
- Show that forcing all terms into one group fails under commutativity constraints.
- This demonstrates that commutativity C is actually constraining the proof search, not just an annotation.

Artifacts
- Model: .\tests\conf_demo_ham_2q.conf
- QProgram: .\tests\demo\qprog_B_bad1_bad_one_group.txt
- Goal: .\tests\demo\goal_B_bad1_bad_one_group.txt
- Out: .\out_demo_ham_bad_one_group.jsonl

Command (no extra script; write files + run compare-jsonl)
    $Model = ".\tests\conf_demo_ham_2q.conf"
    $qprog = ".\tests\demo\qprog_B_bad1_bad_one_group.txt"
    $goal  = ".\tests\demo\goal_B_bad1_bad_one_group.txt"
    $out   = ".\out_demo_ham_bad_one_group.jsonl"

    @'
    [
      Clause {negAtoms = [], posAtoms = [Atom "Q" [TFun "a" []]]},
      Clause {negAtoms = [], posAtoms = [Atom "R" [TFun "a" []]]},
      Clause {negAtoms = [], posAtoms = [Atom "S" [TFun "a" []]]},
      Clause {negAtoms = [], posAtoms = [Atom "T" [TFun "a" []]]},
      Clause {negAtoms = [Atom "Q" [TFun "a" []], Atom "R" [TFun "a" []], Atom "S" [TFun "a" []], Atom "T" [TFun "a" []]], posAtoms = [Atom "G1" [TFun "a" []]]}
    ]
    '@ | Set-Content -Encoding utf8NoBOM -Path $qprog

    'Goal {wantPos = [Atom "G1" [TFun "a" []]], wantNeg = []}
    ' | Set-Content -Encoding utf8NoBOM -Path $goal

    Remove-Item $out -Force -ErrorAction SilentlyContinue
    stack exec qlp -- --model $Model --comm pauli --max-sol 20 --compare-jsonl $out $qprog $goal | Out-Null
    (Get-Content $out | Where-Object { $_.Trim() } | Select-Object -Last 1)

Expected result (last line in Out)
- verdict should be not-applicable
- sols_hilbert should be []
- sols_always may still contain ["{}"] because comm=always ignores commutativity constraints
