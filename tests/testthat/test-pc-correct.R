test_that("PC.correct removes variance captured by the leading PCs", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  source_bm <- create_filebacked_matrix(mat, tmp, base_name = "pc_source")
  pcs_to_keep <- 2L
  pca_result <- big.PCA(
    source_bm,
    pcs.to.keep = pcs_to_keep,
    thin = FALSE,
    verbose = FALSE,
    delete.existing = TRUE
  )

  corrected_desc <- PC.correct(
    pca_result,
    source_bm,
    num.pcs = 1,
    pref = "pc_corrected",
    write = FALSE,
    tracker = FALSE,
    verbose = FALSE
  )
  corrected <- get.big.matrix(corrected_desc)
  corrected_matrix <- bigmemory::as.matrix(corrected)

  expect_equal(dim(corrected_matrix), dim(mat))
  expect_equal(rownames(corrected_matrix), rownames(mat))
  expect_equal(colnames(corrected_matrix), colnames(mat))
  expect_true(all(abs(rowMeans(corrected_matrix)) < 1e-10))

  pc1 <- pca_result$PCs[, 1]
  correlations <- apply(
    corrected_matrix,
    1,
    function(row) {
      if (sd(row) == 0) {
        0
      } else {
        stats::cor(row, pc1)
      }
    }
  )
  expect_true(all(abs(correlations) < 1e-6))
  expect_gt(max(abs(corrected_matrix - mat)), 1e-6)
})
