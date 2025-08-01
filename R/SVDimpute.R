#' SVD Imputation Algorithm for Matrix Completion
#'
#' @description
#' Replace `NA` values in matrix `X` with imputed values.  Standardisation is done down the columns, and imputation along the rows, because the typical set-up would be variables in the columns and cases in the rows.  If your `X` is the other way around, then use `transpose = TRUE`.  _The ordering of the columns matters_ because Last Value Carried Forwards (and then Next Value Carried Backwards) is used to initialize the `NA`s in each row.
#'
#' Although `k` can be specified directly, it is safer to set it indirectly using `pmin`, see Details.
#'
#' @param X Numeric matrix containing missing values as `NA`.
#' @param k Desired rank.
#' @param pmin Minimum proportional variation, used to set `k` if `k = NULL`, see Details.
#' @param transpose Logical, swap rows and columns internally.
#'
#' @details
#' `X` cannot have a whole row or a whole column of missing values. `NA`s in `X` are initially filled using row effects, after the columns of `X` have been standardized.
#'
#' If `k` is not specified, the value of `k` is calculated from the SVD of filled `X` after it has been centered and scaled.  `k` is the smallest value for which the proportion of variation is at least `pmin`.
#'
#' @returns A matrix like `X` but with the `NA`s imputed, plus an attribute `k`, the rank.
#'
#' @references Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown, Trevor Hastie, Robert Tibshirani, David Botstein, Russ B. Altman, Missing value estimation methods for DNA microarrays, Bioinformatics, Volume 17, Issue 6, June 2001, Pages 520–525, <https://doi.org/10.1093/bioinformatics/17.6.520>.
#'

## general purpose check for scalar arguments

is.scalar <- function(x, round = FALSE, positive = FALSE, strict = TRUE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x)
  if (isTRUE(round)) ok <- ok && (x == round(x))
  if (isTRUE(positive)) {
    ok <- ok && ifelse(isTRUE(strict), x > 0, x >= 0)
  }
  ok
}

LVCF <- function(x) {
  nna <- !is.na(x)
  if (all(nna)) return(x)
  y <- x[nna][cumsum(nna)]
  c(rep(NA, length(x) - length(y)), y)
}

NVCB <- function(x) {
  rev(LVCF(rev(x)))
}

up_down <- function(x) {
  NVCB(LVCF(x))
}

#' @export

SVDimpute <- function(X, k = NULL, pmin = 1 - 1E-4, maxit = 5,
  transpose = FALSE) {

  ## these are just to clarify error messages

  is.nnint <- function(x) {
    is.scalar(x, round = TRUE, positive = TRUE, strict = FALSE)
  }

  is.posint <- function(x) {
    is.scalar(x, round = TRUE, positive = TRUE)
  }

  stopifnot(is.matrix(X), is.numeric(X), is.nnint(maxit))
  transpose <- isTRUE(transpose)
  if (transpose) X <- t(X)

  na_mask <- is.na(X)
  if (!any(na_mask)) return(if (transpose) t(X) else X)
  na_row <- apply(na_mask, 1L, all)
  na_col <- apply(na_mask, 2L, all)
  if (any(na_row) || any(na_col)) {
    stop("cannot impute with entire row or column missing")
  }

  ## infill with LVCF/NVCB

  Y <- X
  Y[] <- t(apply(Y, 1L, up_down))

  ## rescale, no NAs now

  xmn <- colMeans(Y)
  Y[] <- sweep(Y, 2L, xmn, "-")
  xsd <- zapsmall(sqrt(colMeans(Y * Y)))
  xsd <- ifelse(xsd == 0, 1, xsd)
  Y[] <- sweep(Y, 2L, xsd, "/") # now standardized

  ## find k, check rank

  decomp <- svd(Y, nu = 0, nv = 0)
  d <- zapsmall(decomp$d^2)
  r <- sum(d > 0)
  if (is.null(k)) {
    stopifnot(is.scalar(pmin), 0 < pmin, pmin < 1)
    d <- cumsum(d) / sum(d)
    k <- which.max(d >= pmin)
  } else {
    stopifnot(is.posint(k))
  }
  if (k >= r) {
    stop(sprintf("k (%i) not smaller than rank (%i)", k, r))
  }

  ## iterate row completion

  na <- which(na_mask, arr.ind = TRUE)
  ivals <- unique(na[, 1L])
  na_list <- lapply(ivals, function(i) {
    list(i = i, j = na[na[, 1L] == i, 2L])
  })

  ## here we go

  for (iter in seq_len(maxit)) {
    U <- svd(Y, nu = k, nv = 0)$u # thin SVD
    for (rw in na_list) {
      i <- rw$i
      for (j in rw$j) {
        coff <- qr.coef(qr(U[-i, , drop=FALSE]), Y[-i, j]) # k vector
        val <- drop(crossprod(U[i, ], coff))
        if (!is.na(val)) Y[i, j] <- val
      }
    }
  }

  ## package and return

  X <- sweep(Y, 2L, xsd, "*")
  X[] <- sweep(X, 2L, xmn, "+")
  if (transpose) X <- t(X)
  attr(X, "param") <- list(transpose = transpose, pmin = pmin,
    k = k, rank = r, maxit = maxit)
  X
}

