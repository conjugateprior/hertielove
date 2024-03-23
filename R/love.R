#' Make German Grades from Grades Out of 100
#'
#' German grades start at 1 (the best) and descend to 5 in uneven increments:
#' 1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4, 5. If you have grades out of 100
#' this function Germanizes them.
#'
#' This function assigns german grades to percentage grades. It's loosely
#' based on a nasty excel macro of unknown provenance whose correctness I
#' take no  responsibility
#' for. Fortunately it also doesn't matter much, because Hertie's
#' grade submission spreadsheet does the calculation itself.
#'
#'
#' @param g grade from a grade out of 100
#' @return The corresponding 'German' grades
#' @export
#'
#' @examples
#'   table(degrade(50:100))
#'
degrade <- function(g){
  th <- c(95, 90,  85,  80, 75,  70,  65, 60,  55,  50, -1)
  de <- c(1,  1.3, 1.7, 2,  2.3, 2.7, 3,  3.3, 3.7, 4,  5)
  sapply(g, function(x) de[min(which(x > th))])
}

na.replace <- function(d, repl = "."){
  d[is.na(d)] <- repl
  d
}

#' Apply the Hertie Love to a Grade Distribution
#'
#' Assume students have raw grades on a 100 point scale.
#' Hertie wants the 0.05, 0.5, and 0.95 quantiles of the empirical
#' to be 70, 85, and 95 respectively. Probably they are not. Hence this
#' function to make it so.
#'
#' First, estimate student ability on a log-odds scale as
#' \deqn{\hat{\theta} = \log(p/(1-p))}
#' where \eqn{p = r/100}.
#'
#' Now consider the 2PL IRT model in which the expected raw score is
#' answering correctly was
#' \deqn{\text{E}[r] = 100 \cdot 1/(1+\exp(- (\theta - \alpha ) \beta))}
#' where \eqn{\alpha} and  \eqn{\beta}
#' are shared difficulty and specificity parameters.
#' Now rather than estimating the model parameters from r,
#' set \eqn{\theta = \hat{\theta}} and optimize  \eqn{\alpha} and  \eqn{\beta}
#' so that the expected raw scores have Hertie's prefered quantile values.
#' The expected raw scores under the fitted model are the new grades.
#'
#' If plot is TRUE then various before and after comparisons are plotted.
#' The second bar graph plots the implications for German grades of the
#' curving. To apply a German grade transformation to any 1-100 grade
#' distribution, use the `degrade` function.
#'
#' After optimisation, the sum squared error (between new to desired grade
#' quantiles) is reported, along with the difficulty and slope parameters of the
#' IRT model, a quantile comparison to see how well we approximate Hertie's
#' preferences, and a before-and-after table in 'German grades'.
#'
#' Limitations: This procedure runs better, that is the final distribution
#' is smoother, if grades are decently variable, mostly over 50 and all less
#' than 100. We could
#' shrink a little to avoid end-effects, but it's seldom been worth the effort.
#'
#' @param r A vector of grades, each out of 100
#' @param plot whether to plot the new grades against originals grades
#'
#' @return a vector of new grades, returned invisibly
#' @importFrom graphics abline barplot par
#' @importFrom stats optim plogis quantile
#' @export
#'
#' @examples
#'   gr <- rbinom(25, prob = 0.66, size = 100)
#'   gr
#'   newgr <- hertielove(gr)
#'   newgr
#'
hertielove <- function(r, plot = FALSE){
  if (max(r) <= 1)
    stop("Grades should be between 1 and 100")

  degr <- c(1,  1.3, 1.7, 2,  2.3, 2.7, 3,  3.3, 3.7, 4,  5)
  qprobs <- c(0.05, 0.5, 0.95)
  qreq <- c(0.70, 0.85, 0.95) # required quantiles for qprobs

  r <- r / 100
  orig <- quantile(r,  probs = qprobs)

  lg <- log(r) - log(1 - r) # simple ability estimates
  f <- function(diffandslope) {
    # exp because slope should be positive
    pp <- plogis((lg - diffandslope[1]) * exp(diffandslope[2]))
    qnew <- quantile(pp, probs = qprobs)
    sum((qnew - qreq)^2) # squared loss - to minimize
  }
  diffandslope <- c(0, 1)
  res <- optim(diffandslope, f)
  if (res$convergence != 0)
    stop("No convergence!")

  cat("Model:\n")
  cat("SSE   =", res$value, "\n")
  cat("diff  =", res$par[2], "\n")
  cat("slope = ", exp(res$par[1]), "\n")

  pp <- plogis((lg - res$par[1]) * exp(res$par[2]))
  qnew <- quantile(pp, probs = qprobs)
  pp <- pp * 100 # back to percentages

  df <- data.frame(rbind(orig, qreq, qnew))*100
  dimnames(df) <- list(c("before", "ideal", "after"),
                       quantiles = c("lower", "median", "upper"))
  cat("\nQuantile comparison:\n")
  print(df, digits = 4)

  cat("\nGerman grade comparison:\n")
  bef <- table(degrade(r * 100))
  aft <- table(degrade(pp))
  fr <- data.frame(DE = degr) |>
    merge(data.frame(DE = names(bef),
                     before = as.numeric(bef)), all.x = TRUE) |>
    merge(data.frame(DE = names(aft),
                     after = as.numeric(aft)), all.x = TRUE)
  print(na.replace(fr), row.names = FALSE)

  fr <- na.replace(fr, 0)

  if (plot) {
    oldpar <- par("mfrow")
    par(mfrow = c(1,2))
    plot(r * 100, pp, xlim = c(40, 100), ylim = c(40, 100),
         xlab = "Original", ylab = "Hertie-loved")
    abline(a = 0, b = 1, col = "grey")

    barplot(t(as.matrix(fr[,2:3])), names.arg = fr[,1], beside = TRUE)
    par(mfrow = oldpar)
  }
  invisible(pp)
}

