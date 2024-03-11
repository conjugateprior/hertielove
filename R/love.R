

#' DE Grader
#'
#' @param g grade from 0 to 100
#'
#' @return The corresponding 'German grade'
#' @export
#'
#' German grades start at 1 (the best) and descend to 5 in uneven increments:
#' 1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3. 7, 4 5.
#'
#' This function is a direct implementation of a Hertie's Excel macro.
#'
#' @examples
#'   table(degrader(0:100))
#'
degrader <- function(g){
  v <- ifelse(g>95,1,
              ifelse(g>90,1.3,
                     ifelse(g>85,1.7,
                            ifelse(g>80,2,
                                   ifelse(g>75,2.3,
                                          ifelse(g>70,2.7,
                                                 ifelse(g>65,3,
                                                        ifelse(g>60,3.3, 0))))))))
  h <- ifelse(g<1,0,
              ifelse(g<50,5,
                     ifelse(g<56,4,
                            ifelse(g<61,3.7,0))))
  v + h
}

#' Apply Hertie Love to a Grade Distribution
#'
#' @param r A vector of grades (0 to 100)
#' @param plot whether to plot the new grades against pre-loved grades
#'
#' @return a vector of new grades (0 to 100), returned invisibly
#' @export
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
#' statistics in the new grades.
#'
#' The alpha parameter is probably of most interest as it represents the
#' general level of question difficulty. Values far from zero indicate that
#' questions are much too easy, or more likely, much too hard (relative to
#' probable actual abilities).
#'
#' @examples
#'   gr <- rbinom(25, prob = 0.66, size = 100)
#'   newgr <- hertielove(gr)

hertielove <- function(r, plot = FALSE){
  qprobs <- c(0.05, 0.5, 0.95)
  qreq <- c(0.70, 0.85, 0.95) # required quantiles for qprobs

  if (max(r) > 1) # assume we're 0-100
    r <- r / 100

  orig <- quantile(r,  probs = qprobs)
  lg <- log(r) - log(1 - r) # simple ability estimates
  f <- function(alphabeta) {
    # exp because slope should be positive
    pp <- plogis(alphabeta[1] + exp(alphabeta[2]) * lg)
    qnew <- quantile(pp, probs = qprobs)
    sum((qnew - qreq)^2) # squared loss - to minimize
  }
  alphabeta <- c(0, 0)
  res <- optim(alphabeta, f)
  if (res$convergence != 0)
    stop("No convergence!")
  cat("SSE =", res$value, "\n")
  cat("alpha =", res$par[1], "\n")
  cat("beta = ", exp(res$par[2]), "\n")

  pp <- plogis(res$par[1] + exp(res$par[2]) * lg)
  qnew <- quantile(pp, probs = qprobs)
  df <- data.frame(rbind(orig, qreq, qnew))
  dimnames(df) <- list(c("orig", "req.", "new"),
                       quantiles = c("lower", "median", "upper"))
  print(df, digits = 4)

  pp <- pp * 100 # back to percentages

  if (plot) {
    plot(r * 100, pp, xlim = c(0, 100), ylim = c(0, 100),
         xlab = "Pre-loved", ylab = "Hertie loved")
    abline(a = 0, b = 1, col = "grey")
  }
  invisible(pp)
}


