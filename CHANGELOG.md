# Changelog for vcs-ignore

## v0.0.3.0 (in development)
- Bump _LTS Haskell_ to `24.57`
- Add a lazy, path-preserving Git ignore matcher for pruning directory walkers
- Add correct last-match precedence evaluation for lazy Git ignore rules
- Add a lazy-versus-eager benchmark for ignored large trees
- Correct slashless pattern depth, `.git/info/exclude`, and exact `.gitignore`
  discovery semantics
- Document negated patterns, match precedence, `gitIgnoreMatcherRoot`, and how
  to run the benchmark in _README_
- Replace _brittany_ with _fourmolu_ as the code formatter
- Modernize the CI workflow
- Update project URLs to `github.com/xwinus/vcs-ignore`

## v0.0.2.0 (2021-12-29)
- minor fixes and improvements
- Bump _LTS Haskell_ to `18.20`

## v0.0.1.0 (2021-05-10)
- initial release
