# bigpca

Utilities for performing PCA workflows on [`bigmemory`](https://cran.r-project.org/package=bigmemory)
file-backed matrices. The package includes helpers for importing large tabular data into
`big.matrix` objects, thinning strategies, previews, and the core `big.PCA()` pipeline with
optional PC-based corrections.

## Dependency baseline (2024 refresh)

The dependency stack has been audited and updated to current CRAN releases:

| Package          | Minimum version | Notes |
| ---------------- | ---------------- | ----- |
| reader           | 1.0.6            | Fast TSV reader retained for backwards compatibility. |
| NCmisc           | 1.1.6            | Supplies preview helpers and utilities. |
| bigmemory        | 4.6.1            | Core shared-memory and file-backed matrices. |
| biganalytics     | 1.1.31           | Convenience analytics that supplement bigmemory. |
| bigmemory.sri    | 0.1.3            | Shared-memory reference implementations. |
| irlba            | 2.3.5            | Preferred partial SVD backend. |

The package now targets R 4.1 or later as the execution baseline. A tuned BLAS/LAPACK implementation
(e.g. OpenBLAS, ATLAS, or Intel MKL) is strongly recommended for best performance with the bigmemory toolchain.

## Reproducing the tested environment

The repository now ships with an [`renv.lock`](renv.lock) file that pins the versions above.
To recreate the development library:

```r
install.packages("renv")
renv::restore(lockfile = "renv.lock")
```

This will install the audited dependency set into a project-local `renv/library` folder
without touching your user library. After restoring, load the package source with
`devtools::load_all()` or build with `R CMD build`. The traditional workflow of installing
from source without renv remains supported; the lockfile simply documents the tested
combination.

## Optional acceleration

The package continues to work without the archived `bigalgebra` extension. When the
package is installed manually, `big.PCA()` will detect it via the `irlba` backend and
use faster matrix multiplications automatically. See `?big.algebra.install.help` for a
short summary of the current recommendation.

## Running checks

With the dependencies in place you can run the standard package checks:

```r
R CMD build .
R CMD check bigpca_*.tar.gz
```

CI hooks in this repository will execute `R CMD check` automatically when the `finish`
utility is run inside the development environment.
