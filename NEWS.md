# bigpca 1.2.0

- Raised the minimum supported R version to 4.1 and refreshed the core dependency
  stack (reader, NCmisc, bigmemory, biganalytics, bigmemory.sri, irlba).
- Replaced the legacy `svn.bigalgebra.install()` workflow with a lightweight helper
  that documents optional `bigalgebra` usage and avoids self-installation side effects.
- Updated `big.PCA()` to use the modern `irlba` backend when available while cleanly
  falling back to base `svd()` when it is not.
- Added an `renv.lock` file and README guidance for recreating the tested dependency set.
- Expanded README content and refreshed `.gitignore` entries for project tooling.
