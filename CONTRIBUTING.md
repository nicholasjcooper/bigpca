# Contributing to bigpca

Thank you for taking the time to improve `bigpca`. The project follows standard
R package conventions and now ships a reproducible automated test suite.

## Development environment

1. Install the locked dependency set with [`renv`](https://rstudio.github.io/renv/):
   ```r
   install.packages("renv")
   renv::restore(project = ".")
   ```
2. Work inside a throwaway branch and keep changes focused.

## Running tests

The package uses [`testthat`](https://testthat.r-lib.org) for automated testing.
All fixtures are synthetic and any file-backed `big.matrix` artefacts are stored
inside temporary directories via `withr::local_tempdir()`, ensuring a clean
working tree after each run. Execute the full suite with:

```r
devtools::test()
```

These tests are also executed as part of `R CMD check` and the continuous
integration pipeline, so please make sure they pass locally before opening a
pull request.

## Coding guidelines

- Follow the existing code style in `bigpca.R` and prefer small, focused
  functions.
- Avoid committing generated artefacts such as `.bck`, `.dsc`, or `.RData`
  files; the `.gitignore` already excludes common cases.
- When adding new functionality, accompany it with test coverage whenever
  feasible.
