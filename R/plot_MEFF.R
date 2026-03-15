#' plot_MEFF: plot MEFF figures
#'
#' @description Plot main effects.
#'
#' @export

plot_MEFF <- function() {

  # 2 panels per page, plot dim in inches, text sizes
  par(mfrow = c(1,2), pin = c(2.7,2.7), cex.main = 0.6, cex.axis = 0.7, cex.lab = 0.7)

  # * Main effects ------------------------------------------------------------

  # Loop through years to plot
  for (yy in yy_plot) {

    # SEA LEVEL VS TEMP
    # Mean projections

    for (gg in temps_list_names) {

      col_darker <- rgb( 1, 0, 0, alpha = 0.4, maxColorValue = 1)
      col_paler <- rgb( 1, 0, 0, alpha = 0.2, maxColorValue = 1)

      # * GSAT ------------------------------------------------------------
      plot( design_sa[[gg]][,gg], myem[[gg]]$mean[,paste0("y",yy)],
            type = "l", lwd = 1.2,
            main = paste( "Main effect: sea level at",yy,"vs", gg ),
            xlab = GSAT_lab[[gg]],
            ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
            ylim = sle_lim[[yy]] )
      abline( h = 0 )
      if (i_s == "GLA") {
        abline( h = glacier_cap, col = "darkred", lwd = 0.5, lty = 5)
      }

      polygon( c( design_sa[[gg]][,gg], rev( design_sa[[gg]][,gg] ) ),
               c( myem[[gg]]$mean[ , paste0("y", yy) ] + myem[[gg]]$sd[ , paste0("y", yy) ],
                  rev( myem[[gg]]$mean[ , paste0("y", yy) ] - myem[[gg]]$sd[ , paste0("y", yy) ] ) ),
               border = NA,
               col = col_darker )
      polygon( c( design_sa[[gg]][,gg], rev( design_sa[[gg]][,gg] ) ),
               c( myem[[gg]]$mean[ , paste0("y", yy) ] + 2 * myem[[gg]]$sd[ , paste0("y", yy) ],
                  rev( myem[[gg]]$mean[ , paste0("y", yy) ] - 2 * myem[[gg]]$sd[ , paste0("y", yy) ] ) ),
               border = NA,
               col = col_paler )

      leg_x1 <- min(design_sa[[gg]][,gg])
      leg_x2 <- max(design_sa[[gg]][,gg])
      rect( leg_x1, 0.94*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.9*sle_lim[[yy]][2],
            col = col_darker, border = NA)
      text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.92*sle_lim[[yy]][2], pos = 4, "Mean +/- 1 s.d.")
      rect( leg_x1, 0.8*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.75*sle_lim[[yy]][2],
            col = col_paler, border = NA)
      text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.78*sle_lim[[yy]][2], pos = 4, "Mean +/- 2 s.d.")

    } # GSAT loop

    # * Ice inputs: continuous ------------------------------------------------------------

    # SEA LEVEL VS ICE MODEL PARAMETERS

    col_list <- hcl.colors(length(ice_cont_list), palette = "Dark 3")

    for (pp in ice_cont_list) {

      col_rgb <- col2rgb( col_list[ which(ice_all_list == pp, arr.ind = TRUE)] )
      col_darker <- rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L], alpha = 0.4 * 255, maxColorValue = 255)
      col_paler <- rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L], alpha = 0.2 * 255, maxColorValue = 255)

      plot( design_sa[[pp]][,pp],
            myem[[pp]]$mean[,paste0("y",yy)],
            type = "l", lwd = 1.2,
            main = paste("Main effect: sea level at",yy,"vs", pp),
            ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
            xlab = pp, ylim = sle_lim[[yy]])
      abline( h = 0 )
      if (i_s == "GLA") {
        abline( h = glacier_cap, col = "darkred", lwd = 0.5, lty = 5)
      }

      polygon( c( design_sa[[pp]][,pp],
                  rev( design_sa[[pp]][,pp] ) ),
               c( myem[[pp]]$mean[ , paste0("y", yy) ] + myem[[pp]]$sd[ , paste0("y", yy) ],
                  rev( myem[[pp]]$mean[ , paste0("y", yy) ] - myem[[pp]]$sd[ , paste0("y", yy) ] ) ),
               border = NA,
               col = col_darker )
      polygon( c( design_sa[[pp]][,pp],
                  rev( design_sa[[pp]][,pp]) ),
               c( myem[[pp]]$mean[ , paste0("y", yy) ] + 2 * myem[[pp]]$sd[ , paste0("y", yy) ],
                  rev( myem[[pp]]$mean[ , paste0("y", yy) ] - 2 * myem[[pp]]$sd[ , paste0("y", yy) ] ) ),
               border = NA,
               col = col_paler )

      leg_x1 <- min(design_sa[[pp]][,pp])
      leg_x2 <- max(design_sa[[pp]][,pp])
      rect( leg_x1, 0.94*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.9*sle_lim[[yy]][2],
            col = col_darker, border = NA)
      text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.92*sle_lim[[yy]][2], pos = 4, "Mean +/- 1 s.d.")
      rect( leg_x1, 0.8*sle_lim[[yy]][2], leg_x1 + 0.1*(leg_x2 - leg_x1), 0.75*sle_lim[[yy]][2],
            col = col_paler, border = NA)
      text( leg_x1 + 0.1*(leg_x2 - leg_x1), 0.78*sle_lim[[yy]][2], pos = 4, "Mean +/- 2 s.d.")

    } # param list

    # Skip to next year plot if no factors
    if (length(ice_factor_list) == 1 && is.na(ice_factor_list)) next

    col_list <- hcl.colors(length(ice_factor_list), palette = "Dark 3")

    # * Ice inputs: factors ------------------------------------------------------------

    for (pp in ice_factor_list) {

      plot( 1:3, 1:3, type = "n", xaxt = "n",
            main = paste("Main effect: sea level at",yy,"vs", pp),
            ylab = paste("Sea level contribution at",yy,"(cm SLE)"),
            xlab = pp, xlim = c(0,length(ice_factor_values[[pp]])+1), ylim = sle_lim[[yy]], xaxs = "i")
      axis(side = 1, at = 1:length(ice_factor_values[[pp]]), labels = ice_factor_values[[pp]],
           cex.axis = 0.6, las = 3)
      abline( h = 0 )

      col_rgb <- col2rgb( col_list[ which(ice_factor_list == pp, arr.ind = TRUE)] )
      col_darker <- rgb(col_rgb[1L], col_rgb[2L], col_rgb[3L], alpha = 0.4 * 255, maxColorValue = 255)

      # Levels for factor
      for (ll in 1:length(ice_factor_values[[pp]])) {

        # Box plot: whiskers
        lab <- paste0(pp,":",ice_factor_values[[pp]][ll])

        # Default level; terrible coding... xxx
        # Second row of dummy variable columns in main effects design are all 0s,
        # i.e. corresponds to default level
        if (ll == 1) {
          lab2 <- paste0(pp,":",ice_factor_values[[pp]][2])
          arrows( ll,
                  myem[[lab2]]$mean[ 2, paste0("y", yy) ] - 2 * myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                  ll,
                  myem[[lab2]]$mean[ 2, paste0("y", yy) ] + 2 * myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                  lwd = 1.2, angle = 90, length = 0.05, code = 3, col = col_darker)
          rect( ll - 0.1,
                myem[[lab2]]$mean[ 2, paste0("y", yy) ] - myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                ll + 0.1,
                myem[[lab2]]$mean[ 2, paste0("y", yy) ] + myem[[lab2]]$sd[ 2, paste0("y", yy) ],
                col = "white", border = col_darker)
          lines( ll + 0.1*c(-1,1),
                 rep(myem[[lab2]]$mean[ 2, paste0("y", yy) ], 2), col = col_darker)


        } else { # Other levels

          stopifnot(lab %in% ice_dummy_list)

          arrows( ll,
                  myem[[lab]]$mean[ 1, paste0("y", yy) ] - 2 * myem[[lab]]$sd[ 1, paste0("y", yy) ],
                  ll,
                  myem[[lab]]$mean[ 1, paste0("y", yy) ] + 2 * myem[[lab]]$sd[ 1, paste0("y", yy) ],
                  lwd = 1.2, angle = 90, length = 0.05, code = 3, col = col_darker)
          rect( ll - 0.1,
                myem[[lab]]$mean[ 1, paste0("y", yy) ] - myem[[lab]]$sd[ 1, paste0("y", yy) ],
                ll + 0.1,
                myem[[lab]]$mean[ 1, paste0("y", yy) ] + myem[[lab]]$sd[ 1, paste0("y", yy) ],
                col = "white", border = col_darker)
          lines( ll + 0.1*c(-1,1),
                 rep(myem[[lab]]$mean[ 1, paste0("y", yy) ], 2), col = col_darker)

        }
      } # factor levels
    } # ice_factor_list

  } # Year loop

}
