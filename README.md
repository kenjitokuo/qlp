# qlp
## Hilbert backend (finite-dimensional commutativity check)

The executable can read a small preset file to assign each predicate symbol to a fixed 2x2 projection operator, and uses the commutator norm to test commutativity.

Default:
  stack run

Specify a model file:
  stack run -- --model hilbert.conf

If the file is missing, the program falls back to an internal default model.

### hilbert.conf format (minimal)

dim 2
eps 1e-9
pred P Z0
pred Q Z1
pred R X+
pred S X-

Notes:
- dim is currently 2 only (preset projections).
- eps is the threshold for "commutes".
- pred <Name> <Preset> assigns a predicate name to a preset projection.
