#' make_emu: build emulator
#'
#' @description
#' Build emulators of principal components with RobustGaSP.
#' Based on code by Jonty Rougier.
#'
#' @returns `make_emu()` returns an emulator object to use.
#'
#' @export


# ________________----
# EMULATE ------------------------------------------------------------

# Build emulator -----------------------------------------------------------------------

make_emu <- function(designX, responseF, r = NULL, thresh = 0.999) {

  # ARGUMENTS WHEN CALLED:
  #    designX <- ice_design_scaled
  #    responseF <- as.matrix( ice_data[ , paste0("y", years_em) ] )
  #    r <- NULL
  #    thresh <- 0.99

  cat("_____________________________________\n", file = logfile_build, append = TRUE)
  cat("make_emu: building emulator...\n", file = logfile_build, append = TRUE)

  stopifnot(is.matrix(designX))
  m <- nrow(designX)
  d <- ncol(designX)
  stopifnot(is.matrix(responseF), nrow(responseF) == m)
  n <- ncol(responseF)
  if (!is.null(r))
    stopifnot(r == round(r), 0 < r, r <= n)
  stopifnot(length(thresh) == 1, 0 < thresh, thresh < 1)

  ## SVD

  cc <- colMeans(responseF)

  # Use sweep to centre data (subtract column means from columns of responseF)
  # then do SVD i.e. a PCA
  decomp <- svd(sweep(responseF, 2L, cc, "-"))
  dd2 <- decomp$d^2
  scree <- cumsum(dd2) / sum(dd2)
  if (is.null(r))
    r <- which.max(scree >= thresh) # first exceedance
  U <- decomp$u[, 1L:r, drop=FALSE]
  Vt <- (decomp$d * t(decomp$v))[1L:r, , drop=FALSE]

  ## write a message

  cat(sprintf("make_emu: r = %i, scree = %.1f%%\n", r, 100 * scree[r]), file = logfile_build, append = TRUE)

  ## build emulators, hide rgasp output

  sink(file = paste0(outdir,out_name,"_", emulator_type, ".log"))

  if ( emulator_type == "statGP") {

    # Get all inputs for trends in RobustGaSP
    trendX <- designX

    # Drop factors (dummy variable columns)
    if ( include_factors) {

      cat("\nDropping factors from trend:\n", file = logfile_build, append = TRUE)
      cat(paste(c(ice_dummy_list, "\n"), collapse = " "), file = logfile_build, append = TRUE)

      trendX <- trendX[ , input_cont_list]

      cat("\nKeeping:\n", file = logfile_build, append = TRUE)
      cat(paste(c(colnames(trendX), "\n"), collapse = " "), file = logfile_build, append = TRUE)
    }
  }

  # Emulator model for each principal component
  EMU <- lapply(1L:r, function(j) {

    cat(sprintf("\nTraining emulator for PC %i\n", j), file = logfile_build, append = TRUE)

    if (emulator_type == "statGP") {
      emu_pc <- RobustGaSP::rgasp(design = designX, response = U[, j], trend = cbind(1, trendX),
                                  nugget.est = TRUE , lower_bound = lower_bound,
                                  kernel_type = kernel, alpha = rep(alpha, dim(as.matrix(designX))[2]))
    }

    if (emulator_type == "deepgp") {
      emu_pc <- deepgp::fit_one_layer( designX, U[, j], cov = emulator_covar, nmcmc = N_mcmc )
    }

    # LaGP is structured differently to other GPs:
    # Here we are just training on a random sample to estimate initial separable length scales
    # Actual training for prediction is in predict function (because train and predict are done simultaneously)
    # Adapted from laGP vignette
    if (emulator_type == "laGP") {

      # Pre-scaling is quite slow
      if (laGP_scaling) {

        # Index for random subsample of simulations
        subs <- sample(1:m, min(1000, m), replace = FALSE)

        # Generates prior on length scale, based on distribution of distances between design points
        # Upper bound at 100
        d2 <- laGP::darg(list(mle = TRUE, max = 100), designX)
        cat(sprintf("\nLength scale prior value: %.3f\n", d2$start), file = logfile_build, append = TRUE)

        # Local approximate GP object with separable correlation structure
        # based on random sample of simulations
        # setting priors on length scales as above
        gpsepi <- laGP::newGPsep( designX[subs, ], U[subs, j],
                                  d = rep(d2$start, ncol(designX)), g = 0.1, # g = 1/1000, # might try larger initial nugget again later: 1/10
                                  dK = TRUE)

        # Maximum likelihood estimation of the length scales
        that <- laGP::mleGPsep(gpsepi, param = "d", tmin = d2$min, tmax = d2$max,
                               ab = d2$ab, maxit = 200)

        cat("\nLength scale estimates for each input:\n", file = logfile_build, append = TRUE)

        for (pp in 1:ncol(designX)) {
          cat(sprintf("%s: %.4f\n", colnames(designX)[pp], that$d[pp]), file = logfile_build, append = TRUE)
        }

        # No need to keep the GP object
        laGP::deleteGPsep(gpsepi)

        # Just return length scales for scaling inputs when predicting, not emu object
        emu_pc <- that$d

      } else {

        #     # Testing code
        #     # if (FALSE) {
        #     #   # Use hard-coded for testing
        #     #   if (j==1) ls <- c(33.85, 0.04, 16.91, 16.34, 5.90, 7.81, 100.00)
        #     #   if (j==2) ls <- c(97.14, 0.03, 14.90, 10.70, 4.68, 5.77, 100.00)
        #     #   if (j==3) ls <- c(4.81, 0.04, 10.38, 7.73, 3.95, 4.76, 83.91)
        #     #   if (j==4) ls <- c(19.00, 0.03, 7.66, 4.63, 3.25, 3.72, 100.00)
        #     #   if (j==5) ls <- c(2.64, 0.02, 6.21, 3.58, 3.77, 3.46, 91.40)
        #     #   print(ls)
        #     #   ls
        #     # }

        # Return 1s i.e. no scaling
        emu_pc <- rep(1.0, ncol(designX))

      }

    } # laGP

    emu_pc

  }) # EMU: list of emulator models for each PC (or length scales from laGP)


   if (laGP_scaling) {

     # Quick plot of thetas (box plot for PCs xxx use scatter?)
     if ( emulator_type == "laGP") {

       pdf( file = paste0( outdir, out_name, "_lengthscales.pdf"),
            width = 9, height = 5)

       thats <- matrix(NA, nrow = r, ncol = ncol(designX))
       for (j in 1:r) thats[ j, ] <- EMU[[j]]

       boxplot( thats, main = paste0("Length scales (",r," PCs)"), xlab = "Emulator input",
                ylab = "Length scale")

       dev.off()
     }
  }

  #  sink()


  ## predict method returns a list

  pred_EMU <- function(designXout) {

    if (emulator_type == "statGP") {

      # Trends used in RobustGaSP
      trendXout <- designXout

      # Drop any factors
      if ( include_factors) {
        tt <- which( input_cont_list %in% colnames(ice_design), arr.ind = TRUE )
        trendXout <- trendXout[ , tt, drop = FALSE]
      }

      # Predict for set of new design points using each PC emulator in list
      EMU_pred <- lapply(EMU, function(emu) {

        RobustGaSP::predict( emu, testing_input = designXout,
                             testing_trend = cbind(1, trendXout) )[c("mean", "sd")]
      })
    } # statGP

    if (emulator_type == "deepgp") {
      EMU_pred <- lapply(EMU, function(emu) {
        deepgp::predict( emu, designXout )[c("mean", "s2")]
      })
    }

    if (emulator_type == "laGP") {

      # Loops over PCs to get length scales (if estimated)
      EMU_pred <- lapply(1L:r, function(j) {

        # LaGP builds and predicts at the same time

        # Get full ensemble and prediction designs,
        # responses, and length scales to scale inputs in both designs
        # Latter two are for j-th PC
        designX_scaled <- designX
        designXout_scaled <- designXout
        response <- U[ , j]
        scales <- sqrt(EMU[[j]]) # EMU here are theta; laGP recommends using sqrt(theta)

        # LaGP selects subsample to train with for each new prediction
        # Use prior length scale = 1 because inputs rescaled

        if (laGP_scaling) {

          # Scale each column with sqrt(length scale) returned by earlier EMU object
          for(pp in 1:ncol(designX)) {
            designX_scaled[, pp] <- designX_scaled[, pp] / scales[pp]
            designXout_scaled[, pp] <- designXout_scaled[, pp] / scales[pp]
          }

          # Build and predict from local design, using scaled inputs
          lagp_pred <- laGP::aGP(designX, response, designXout, g = laGP_nugget_prior,
                                 d = list(start = 1, max = 20),
                                 method = laGP_method)[c("mean", "var", "d", "g")]
        } else {

          # Build and predict from local design, using original inputs
          lagp_pred <- laGP::aGP(designX, response, designXout, g = laGP_nugget_prior,
                                 method = laGP_method)[c("mean", "var", "d", "g")]
        }

        lagp_pred

      })

    } # if laGP

    # Return predictions
    EMU_pred

  } # pred_EMU

  ## return a function

  robj <- function(designXout, type = c("mean", "sd", "var", "all")) {

    type <- match.arg(type)
    if (!is.matrix(designXout) && length(designXout) == d) {
      dim(designXout) <- c(1L, d)
    } else {
      stopifnot(is.matrix(designXout), ncol(designXout) == d)
    }
    m_out <- nrow(designXout)
    pplist <- pred_EMU(designXout) # r-list

    ## compute the time series (n years) of emulated mean values
    # for each of the m_out simulations
    # from the r individual PC means
    mu <- sapply(pplist, "[[", "mean") # m_out x r
    dim(mu) <- c(m_out, r)
    mx <- sweep(mu %*% Vt, 2L, cc, "+") # m_out x n

    # Can choose to return only the mean
    if (type == "mean")
      return(list(mean = mx))

    # Output estimated length scales and nuggets, e.g. for testing
    if (emulator_type == "laGP") {
      darg <- sapply(pplist, "[[", "d") # m_out x r
      print(darg)
      garg <- sapply(pplist, "[[", "g") # m_out x r
      print(garg)
    }

    ## compute the sd similarly
    if (emulator_type == "statGP") sdu <- sapply(pplist, "[[", "sd") # m_out x r
    # deepgp and laGP return sd^2 not sd:
    if (emulator_type == "deepgp") sdu <- sqrt(sapply(pplist, "[[", "s2")) # XXX CHECK THESE
    if (emulator_type == "laGP") sdu <- sqrt(sapply(pplist, "[[", "var"))

    dim(sdu) <- c(m_out, r)
    sdx <- lapply(1L:m_out, function(i) {
      sqrt(colSums((sdu[i, ] * Vt)^2)) # n vector
    })
    sdx <- do.call("rbind", sdx) #  m_out x n

    # Can choose to return only the s.d.
    if (type == "sd") return(list(mean = mx, sd = sdx))

    ## compute the variance - i.e. covariances between the n years
    Sx <- lapply(1L:m_out, function(i) {
      as.vector(crossprod(sdu[i, ] * Vt)) # n*n vector
    })
    Sx <- do.call("cbind", Sx) # n*n x m_out
    dim(Sx) <- c(n, n, m_out)
    Sx <- aperm(Sx, c(3, 1, 2))

    # Default is to return mean, sd and var
    return(list(mean = mx, sd = sdx, var = Sx))
  }

  ## class and return

  cat("\nmake_emu: end of emulator build\n",file = logfile_build, append = TRUE)
  cat("_____________________________________\n",file = logfile_build, append = TRUE)

  structure(robj, class = "emu")

}


# END MAKE_EMU()
