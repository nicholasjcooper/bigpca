options(testthat.edition = 3)
options(bigmemory.allow.dimnames = TRUE)

synthetic_matrix <- function() {
  path <- system.file("extdata", "synthetic_matrix.tsv", package = "bigpca", mustWork = TRUE)
  read.table(path, header = TRUE, sep = "\t", row.names = 1, check.names = FALSE) |> as.matrix()
}

create_filebacked_matrix <- function(mat, dir, base_name = "fixture") {
  stopifnot(dir.exists(dir))
  backingfile <- sprintf("%s.bck", base_name)
  descriptorfile <- sprintf("%s.dsc", base_name)
  bm <- bigmemory::filebacked.big.matrix(
    nrow = nrow(mat),
    ncol = ncol(mat),
    dimnames = list(rownames(mat), colnames(mat)),
    backingfile = backingfile,
    descriptorfile = descriptorfile,
    backingpath = dir
  )
  bm[,] <- mat
  bm
}

align_component_signs <- function(reference, candidate) {
  stopifnot(ncol(reference) == ncol(candidate))
  adjusted <- candidate
  for (idx in seq_len(ncol(reference))) {
    direction <- sign(sum(reference[, idx] * candidate[, idx]))
    if (direction == 0) {
      next
    }
    adjusted[, idx] <- adjusted[, idx] * direction
  }
  adjusted
}
