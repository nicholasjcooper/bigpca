# bigpca NEWS

## 1.2.0 - 2024-10-15

- Audited CRAN dependencies and raised minimum versions for `reader`, `NCmisc`,
  `bigmemory`, and `irlba`.
- Removed obsolete dependencies on `biganalytics` and `bigmemory.sri` and
  replaced `biganalytics::apply()` usage with base R functionality.
- Retired the legacy `svn.bigalgebra.install()` workflow; both historical helper
  functions now signal a defunct error with pointers to the updated BLAS
  guidance in the README.
- Added `renv.lock` configuration to capture tested dependency versions and
  documented restore instructions.
- Refreshed documentation to describe the new dependency landscape and tooling
  expectations.
