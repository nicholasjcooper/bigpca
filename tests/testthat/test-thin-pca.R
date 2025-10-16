test_that("thin reduces file-backed matrices", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  source_bm <- create_filebacked_matrix(mat, tmp, base_name = "thin_source")
  thinned <- thin(
    source_bm,
    keep = 3,
    rows = TRUE,
    random = FALSE,
    pref = "thin_rows",
    verbose = FALSE
  )
  thinned_matrix <- bigmemory::as.matrix(thinned)
  expect_equal(nrow(thinned_matrix), 3L)
  expect_equal(ncol(thinned_matrix), ncol(mat))
  reordered <- mat[match(rownames(thinned_matrix), rownames(mat)), colnames(thinned_matrix), drop = FALSE]
  expect_equal(thinned_matrix, reordered)
})


test_that("thin subsets columns from in-memory matrices", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  thinned <- thin(
    mat,
    keep = 0.5,
    rows = FALSE,
    random = FALSE,
    pref = "thin_cols",
    verbose = FALSE
  )
  thinned_matrix <- bigmemory::as.matrix(thinned)
  expect_equal(nrow(thinned_matrix), nrow(mat))
  expect_equal(ncol(thinned_matrix), round(ncol(mat) * 0.5))
  reordered <- mat[rownames(thinned_matrix), match(colnames(thinned_matrix), colnames(mat)), drop = FALSE]
  expect_equal(thinned_matrix, reordered)
})


test_that("big.PCA matches SVD expectations for matrix and big.matrix inputs", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  source_bm <- create_filebacked_matrix(mat, tmp, base_name = "pca_source")
  pcs_to_keep <- 3L

  pca_big <- big.PCA(
    source_bm,
    pcs.to.keep = pcs_to_keep,
    thin = FALSE,
    verbose = FALSE,
    delete.existing = TRUE
  )
  pca_matrix <- big.PCA(
    mat,
    pcs.to.keep = pcs_to_keep,
    thin = FALSE,
    verbose = FALSE,
    delete.existing = TRUE
  )

  expect_s3_class(pca_big, "bigpca_result")
  expect_s3_class(pca_matrix, "bigpca_result")
  expect_false(pca_big$metadata$thin)
  expect_false(pca_matrix$metadata$thin)
  expect_equal(pca_big$metadata$source_dim, c(rows = nrow(mat), cols = ncol(mat)))
  expect_equal(pca_matrix$metadata$source_dim, c(rows = nrow(mat), cols = ncol(mat)))
  expect_true(pca_big$metadata$method %in% c("irlba", "svd", "lapack", "princomp", "eigen"))

  centered <- mat - rowMeans(mat)
  centered[is.na(centered)] <- 0
  svd_ref <- svd(centered)
  expected_eigenvalues <- svd_ref$d[1:pcs_to_keep]^2

  expect_equal(pca_big$eigenvalues[1:pcs_to_keep], expected_eigenvalues, tolerance = 1e-8)
  expect_equal(pca_matrix$eigenvalues[1:pcs_to_keep], expected_eigenvalues, tolerance = 1e-8)
  expect_equal(pca_big$Evalues[1:pcs_to_keep], expected_eigenvalues, tolerance = 1e-8)
  expect_equal(pca_big$PCs, pca_big$scores)

  aligned_big <- align_component_signs(svd_ref$v[, 1:pcs_to_keep, drop = FALSE], pca_big$scores[, 1:pcs_to_keep, drop = FALSE])
  expect_equal(aligned_big, svd_ref$v[, 1:pcs_to_keep, drop = FALSE], tolerance = 1e-6)

  aligned_matrix <- align_component_signs(pca_big$scores[, 1:pcs_to_keep, drop = FALSE], pca_matrix$scores[, 1:pcs_to_keep, drop = FALSE])
  expect_equal(aligned_matrix, pca_big$scores[, 1:pcs_to_keep, drop = FALSE], tolerance = 1e-6)
})


test_that("big.PCA records thinning metadata", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  source_bm <- create_filebacked_matrix(mat, tmp, base_name = "thin_meta")
  result <- big.PCA(
    source_bm,
    pcs.to.keep = 2,
    thin = TRUE,
    keep = 0.5,
    how = "uniform",
    rows = TRUE,
    random = FALSE,
    pref = "thin_meta_pca",
    verbose = FALSE,
    delete.existing = TRUE
  )

  expect_true(result$metadata$thin)
  expect_true(all(c("keep", "how", "rows", "random", "pref") %in% result$metadata$thin_args))
})
