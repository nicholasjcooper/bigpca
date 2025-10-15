# bigpca

Utilities for running large scale principal component analyses on
`bigmemory::big.matrix` objects. The package ships specialised import,
preview, thinning, and decomposition helpers that keep data on disk until it is
required in memory, making exploratory analysis of very wide matrices practical.

## Dependency status (October 2024)

The package has been audited against current CRAN releases and now depends on
modern versions of the bigmemory stack:

- `reader (>= 1.0.10)` for text ingestion.
- `NCmisc (>= 1.1.7)` for large matrix utilities.
- `bigmemory (>= 4.6.1)` for file-backed matrices.
- `irlba (>= 2.3.5)` for accelerated partial SVD.

Legacy dependencies on `biganalytics` and `bigmemory.sri` have been removed.
Optional acceleration through `bigalgebra` remains supported but the deprecated
SVN installation helpers have been retired; see below for up-to-date guidance.

## Optional BLAS acceleration

`bigpca` works out of the box with the reference BLAS provided by R. Installing
an optimised BLAS such as OpenBLAS, ATLAS, or Apple's Accelerate framework can
significantly reduce runtime for very large problems. If you also install the
`bigalgebra` package from CRAN, `bigpca::big.PCA()` will automatically make use
of its matrix multiplications. Refer to the `SystemRequirements` field in the
`DESCRIPTION` file for platform notes and recommended BLAS distributions.

## Reproducible environments with renv

A project-level `renv.lock` file records the tested dependency versions. To
create or restore a development library:

```r
install.packages("renv")
renv::restore(project = ".")
```

This will install the audited package set into a local `renv/library` folder,
leaving your user library untouched.

## Running package checks

After installing dependencies via `renv`, run the usual R package workflow:

```r
R CMD build .
R CMD check bigpca_*.tar.gz
```

The `NEWS.md` file records noteworthy changes, including dependency upgrades and
the removal of the legacy bigalgebra installers.
