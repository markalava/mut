################################################################################
###
### Helper functions for ggplot2
###
################################################################################

###-----------------------------------------------------------------------------
### Format axis labels

## Function to round labels. Pass as argument to 'label' in scale_*_continuous
gg.round.ax.labs <- function(breaks, digits) round(breaks, digits)

###-----------------------------------------------------------------------------
### * Colour Schemes

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

###-----------------------------------------------------------------------------
### * Smoothing splines

smooth.spline2 <- function(formula, data, w = NULL, df, spar = NULL, cv = FALSE,
    all.knots = FALSE, nknots = .nknots.smspl, keep.data = TRUE,
    df.offset = 0, penalty = 1, control.spar = list()
  ,...) {
  mat <- model.frame(formula, data)

  smooth.spline(x = mat[, 2], y = mat[, 1], w = w, df = df, spar = spar, cv = cv
               ,all.knots = all.knots, nknots = nknots, keep.data = keep.data
               ,df.offset = df.offset, penalty = 1, control.spar = control.spar)
}


predictdf.smooth.spline <- function(model, xseq, se, level) {
  pred <- predict(model, xseq)
  data.frame(x = xseq, y = pred$y)
}
