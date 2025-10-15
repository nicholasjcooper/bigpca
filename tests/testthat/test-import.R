test_that("import.big.data handles unlabeled matrices", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  unlabeled_path <- file.path(tmp, "matrix_unlabeled.tsv")
  write.table(mat, file = unlabeled_path, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

  row_file <- file.path(tmp, "rows.txt")
  col_file <- file.path(tmp, "cols.txt")
  writeLines(rownames(mat), row_file)
  writeLines(colnames(mat), col_file)

  desc_default <- import.big.data(unlabeled_path, pref = "default", verbose = FALSE)
  bm_default <- get.big.matrix(desc_default)
  expected_default <- mat
  rownames(expected_default) <- paste0("row", seq_len(nrow(mat)))
  colnames(expected_default) <- paste0("col", seq_len(ncol(mat)))
  expect_equal(bigmemory::as.matrix(bm_default), expected_default)

  desc_files <- import.big.data(
    unlabeled_path,
    rows.fn = row_file,
    cols.fn = col_file,
    pref = "from_files",
    verbose = FALSE
  )
  bm_files <- get.big.matrix(desc_files)
  expect_equal(bigmemory::as.matrix(bm_files), mat)

  desc_vectors <- import.big.data(
    unlabeled_path,
    row.names = rownames(mat),
    col.names = colnames(mat),
    pref = "from_vectors",
    verbose = FALSE
  )
  bm_vectors <- get.big.matrix(desc_vectors)
  expect_equal(bigmemory::as.matrix(bm_vectors), mat)
})


# import.big.data emits informative warnings when embedded names disagree.
test_that("import.big.data warns when embedded dimnames are present", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  labeled_path <- file.path(tmp, "matrix_labeled.tsv")
  write.table(mat, file = labeled_path, sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

  expect_warning(
    {
      desc <- import.big.data(labeled_path, pref = "embedded", verbose = FALSE)
    },
    regexp = NA
  )
  bm <- get.big.matrix(desc)
  expect_equal(bigmemory::as.matrix(bm), mat)
})


test_that("import.big.data validates column ordering", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  labeled_path <- file.path(tmp, "matrix_labeled.tsv")
  write.table(mat, file = labeled_path, sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)

  scrambled <- rev(colnames(mat))
  expect_error(
    import.big.data(
      labeled_path,
      row.names = rownames(mat),
      col.names = scrambled,
      pref = "bad_cols",
      verbose = FALSE
    )
  )
})


test_that("import.big.data assembles row-wise partitions", {
  mat <- synthetic_matrix()
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  withr::local_options(list(deleteFileBacked = TRUE))

  row_groups <- list(1:2, 3:5)
  input_files <- character(length(row_groups))
  row_list <- vector("list", length(row_groups))
  for (idx in seq_along(row_groups)) {
    subset_rows <- row_groups[[idx]]
    chunk <- mat[subset_rows, , drop = FALSE]
    input_files[[idx]] <- file.path(tmp, sprintf("chunk_%d.tsv", idx))
    write.table(chunk, file = input_files[[idx]], sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
    row_list[[idx]] <- rownames(chunk)
  }

  desc <- import.big.data(
    input_files,
    row.names = row_list,
    col.names = colnames(mat),
    pref = "row_partition",
    verbose = FALSE
  )
  bm <- get.big.matrix(desc)
  expect_equal(bigmemory::as.matrix(bm), mat)
})
