#' SVD Imputation Algorithm for Matrix Completion
#'
#' @description
#' Replace `NA` values in matrix `X` with imputed values.  Imputation is along the rows, assuming that all columns of `X` have the same units.
#'
#' _The order of the columns matters!_ The initial infill is by a variant on Last Value Carried Forward (LVCF).  Therefore it is important that the columns follow a sequence.  If the columns represent points in a grid, then use [snake()] to reorder them before imputation.
#'
#' @param X numeric matrix containing missing values as `NA`.
#' @param k rank of approximation to `X`.
#' @param pmin minimum proportional variation, used to set `k` if `k = NULL`, see Details.
#' @param maxit maximum number of iterations.
#'
#' @returns A matrix like `X` but with the `NA`s imputed, plus an attribute `"param"`, a list with `pmin`, `k`, and `rank` if `maxit > 0`.
#'
#' @details This is a two-stage imputation.  Stage 1 is an initial infill of all `NA`s using a variant on LVCF.  If `X[i,j]` is `NA`, it is filled by `X[i,j-1] + diff(X[istar, c(j-1,j)])` where `istar` is the run with the closest value to `X[i,j-1]`.This is followed by a similar backward pass, and the function terminates with an error if there are `NA` remaining after both passes.
#'
#' Stage 2 (if `maxit > 0`) is a SVD imputation, similar to that described in Troyanskaya et al (2001), see References.  This is carried out after centering `X` (now with no `NA`s).  If `k` is not specified, it is the rank of the SVD approximation of `X` which achieves a proportion `pmin` of the variation of `X`.
#'
#' @references Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown, Trevor Hastie, Robert Tibshirani, David Botstein, Russ B. Altman, Missing value estimation methods for DNA microarrays, Bioinformatics, Volume 17, Issue 6, June 2001, Pages 520–525, <https://doi.org/10.1093/bioinformatics/17.6.520>.
#'
#' @seealso LVCF will give a poor initial fill for points on the upper edge of a grid.  If the columns of `X` represent points in a grid, then reorder them using the permutation from [snake()] before imputation.
#'
#' @examples
#' ## demo using diamondEOO
#'
#' Y <- diamondEOO$Y
#' mask <- is.na(Y)
#' show(mean(mask)) # 6% NAs where P < 0, at high values of V
#'
#' ## LVCF only
#'
#' Y1 <- SVDimpute(Y, maxit = 0)
#' show(summary(Y1[mask])) # all small
#' all.equal(Y[!mask], Y1[!mask]) # TRUE
#'
#' ## now with some SVD iterations
#'
#' Y1 <- SVDimpute(Y, maxit = 5) # the default
#' show(summary(Y1[mask])) # all small
#'
#' ## using snake()
#'
#' dim <- lengths(diamondEOO$grid)
#' oo <- snake(dim)
#' Y2oo <- SVDimpute(Y[, oo])
#' Y2 <- Y2oo[, order(oo)]
#' show(summary((Y1 - Y2)[mask])) # minor differences

#' @export

SVDimpute <- function(X, k = NULL, pmin = 1 - 1E-4, maxit = 5) {

  stopifnot(is.matrix(X), is.numeric(X), is.nnint(maxit))

  m <- ncol(X)
  na_mask <- is.na(X)
  if (!any(na_mask)) return(X)
  if (m == 1) stop("Cannot impute a single column")
  na_row <- apply(na_mask, 1L, all)
  na_col <- apply(na_mask, 2L, all)
  if (any(na_row) || any(na_col)) {
    stop("cannot impute with entire row or column missing")
  }

  ## forward pass

  Y <- X
  for (j in 2L:m) {
    if (!anyNA(Y[, j])) next
    pairs <- na.omit(X[, c(j-1L, j), drop=FALSE])
    if (nrow(pairs) == 0) next
    miss <- which(!is.na(Y[, j-1L]) & is.na(Y[, j]))
    istar <- sapply(miss, function(i) {
      which.min(abs(Y[i, j-1L] - pairs[, 1L]))
    })
    Y[miss, j] <- Y[miss, j-1L] + pairs[istar, 2L] - pairs[istar, 1L]
  }

  ## backward pass

  for (j in (m-1L):1) {
    if (!anyNA(Y[, j])) next
    pairs <- na.omit(X[, c(j, j+1L), drop=FALSE])
    if (nrow(pairs) == 0) next
    miss <- which(!is.na(Y[, j+1L]) & is.na(Y[, j]))
    istar <- sapply(miss, function(i) {
      which.min(abs(Y[i, j+1L] - pairs[, 2L]))
    })
    Y[miss, j] <- Y[miss, j+1L] - pairs[istar, 2L] + pairs[istar, 1L]
  }

  if (anyNA(Y)) stop("Too many NAs to impute")

  ## opportunity for early exit

  if (maxit == 0) return(Y)

  ## sweep out means

  xmn <- colMeans(Y)
  Y <- sweep(Y, 2L, xmn, "-")

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

  ## here we go

  na <- which(na_mask, arr.ind = TRUE)
  ivals <- unique(na[, 1L])
  na_list <- lapply(ivals, function(i) {
    list(i = i, j = na[na[, 1L] == i, 2L])
  })

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

  X <- sweep(Y, 2L, xmn, "+")
  attr(X, "param") <- list(pmin = pmin, k = k, rank = r, maxit = maxit)
  X
}

# Helper functions
is.scalar <- function(x, round = FALSE, positive = FALSE, strict = TRUE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x)
  if (isTRUE(round)) ok <- ok && (x == round(x))
  if (isTRUE(positive)) {
    ok <- ok && ifelse(isTRUE(strict), x > 0, x >= 0)
  }
  ok
}

is.nnint <- function(x) {
  is.scalar(x, round = TRUE, positive = TRUE, strict = FALSE)
}

is.posint <- function(x) {
  is.scalar(x, round = TRUE, positive = TRUE)
}

