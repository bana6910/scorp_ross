# This script computes rmspe

rmspe <- function(truth_y, pred_y) {
  nonzero <- truth_y > 0
  n <- sum(nonzero)
  diff <- (truth_y[nonzero] - pred_y[nonzero]) / truth_y[nonzero]
  diff <- diff**2
  diff <- sqrt(sum(diff)/n)
  return(diff)
}

# Test rmspe on some samples to check that it's correct
actuals <- c(1000, 10000, 10, 0)
pred    <- c(900, 9000, 9, 0)
rmspe(actuals, pred)

actuals <- c(1000, 10000, 10, 0)
pred    <- c(500, 1000, 9, 0)
rmspe(actuals, pred)
