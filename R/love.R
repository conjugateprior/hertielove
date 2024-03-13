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
#' This function figures out which 2PL IRT model would give the
#' required Hertie grade distribution, assuming if all questions
#' share the same difficulty parameter (constrained to be positive).
#'
#' Hertie requires that (no more than) five percent of the class
#' score below 70, (not more than) five percent of the class score
#' above 95, and the class median grade is 85.
#'
#' The function reports SSE: how close it got to fitting those constraints,
#' alpha and beta, the back estimated IRT parameters, and a comparison table
#' of the original grade statistics, the required statistics, and those
#' statistics in the new grades
#'
#' The alpha parameter is probably of most interest as it represents the
#' general level of question difficulty. Values far from zero indicate that
#' questions are much too easy, or more likely, much too hard (relative to
#' probable actual abilities).
#'
#' @param r A vector of grades, each from 1 to 100
#' @param plot whether to plot the new grades against originals grades
#'
#' @return a vector of new grades, each from 1 to 100, returned invisibly
#' @importFrom graphics abline
#' @importFrom stats optim plogis quantile
#' @export
#'
#' @examples
#'   p <- rbeta(25, 6, 4)
#'   ab <- qlogis(p)
#'   qd <- qlogis() # ~0.7
#'
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

