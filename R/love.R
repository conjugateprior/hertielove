#' DE-grade
#'
#' German grades start at 1 (the best) and descend to 5 in uneven increments:
#' 1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4, 5.
#'
#' This function assigns german grades to percentage grades.
#'
#'
#' @param g grade from 1 to 100
#' @return The corresponding 'German grade'
#' @export
#'
#' @examples
#'   table(degrade(0:100))
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

#' Apply Hertie Love to a Grade Distribution
#'
#' Students have raw grades in vector r on a 100 point scale.
#' Rescale this to 0,1 and treat this new score p as the proportion of
#' questions they answered correctly. The empirical distribution of p
#' has the 0.05, 0.5, and 0.95 quantiles; label these q_1, q2, q3.
#' Hertie suggests that q1 should be at 70, q2 should be at 85, and
#' q3 should be at 95. Probably they are not.
#'
#' Estimate student ability on a log-odds scale as l = log(p/(1-p)).
#' If these estimates were from a 2PL IRT model in which the log odds of
#' answering correctly was (a - d) b where a is ability, d difficulty,
#' and b the specificity of every question.  Normally we would take the
#' student scores, estimate the d and b parameters and infer a. To
#' curve the student scores we will set a = l and choose d and b parameters
#' to have the preferred quantiles noted above (the optimisation treats
#' the sum of squared differences between the quantiles implied by a
#' particular setting of d and b, and q1, q2, and q3.
#'
#' If plot is TRUE then various before and after comparisons are plotted.
#' The second bar graph plots the implications for German grades of the
#' curving. To apply a German grade transformation to any 1-100 grade
#' distribution, use the `degrade` function.
#'
#' @param r A vector of grades, each from 1 to 100
#' @param plot whether to plot the new grades against originals grades
#'
#' @return a vector of new grades, each from 1 to 100, returned invisibly
#' @importFrom graphics abline barplot par
#' @importFrom stats optim plogis quantile
#' @export
#'
#' @examples
#'   gr <- rbinom(25, prob = 0.66, size = 100)
#'   newgr <- hertielove(gr)
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

