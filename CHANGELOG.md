# Changelog for vcs-ignore

## v0.1.0.0 (in development)

- Replace the eager repository scan and separate lazy matcher with one opaque,
  lazy `GitRepository` session
- Add kind-aware ignore queries with consistent Git last-match and
  ignored-parent semantics
- Add a constant-result-memory repository fold with caller pruning and global
  early termination
- Never follow symbolic links during traversal and expose each entry's kind
- Support Git directories, gitfiles, linked worktrees, and common directories
- Hide pattern and raw filesystem implementation details from the public API
- Remove the `Repo` type class, `scanRepo`, eager `Git`, and the separate
  `GitIgnoreMatcher` API
- Update the executable, documentation, tests, and benchmarks for the new API
- Document intentionally unsupported Git configuration (`core.excludesFile`,
  `core.ignoreCase`, and conditional includes) and sparse-checkout index rules

## v0.0.2.0 (2021-12-29)
- minor fixes and improvements
- Bump _LTS Haskell_ to `18.20`

## v0.0.1.0 (2021-05-10)
- initial release
