
ab <- c(18.2,3.4)
x <- seq(0, 1, by = 0.01)
plot(x, dbeta(x, ab[1], ab[2]), type = "n")
thr <- qbeta(p = c(0.05, 0.5, 0.95), ab[1], ab[2])
rbind(c(70, 85, 95), thr)
abline(v = thr, lty = "dotted")
lines(x, dbeta(x, ab[1], ab[2]), type = "h")

sc <- round(100 / (1 + exp(-(rnorm(25, mean = 0, sd = 1) + 0.3))))
#dd <- dbeta(x, ab[1], ab[2])
#dd <- dd / sum(dd)
#dd <- dd[1:100]

newsc <- hertielove2(sc/100)
car::qqPlot(newsc, dist = "beta", shape1 = 18.2, shape2 = 3.4,
            envelope = list(border = FALSE),
            ylab = "Transformed grades / 100")

plot(density(rbeta(10000, 19, 3.4)), main = "Ideal vs transformed grade density")
lines(density(newsc, kernel = "optcosine"), col = "red")


ambeta <- function(ab){ (ab[1] - 1/3) / (sum(ab) - 2/3) }
mbeta <- function(ab){ (ab[1]) / (sum(ab)) }

gg <- function(ab) { sum((qbeta(p = c(0.05, 0.5, 0.95),
                                ab[1], ab[2]) - c(.70, .85, .95))^2) }


# expected proportions of each german grade
deprops <- function(ab){
  gr <- c(0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.65,
          0.6, 0.56, 0.5, 0.01)
  g <- pbeta(gr, ab[1], ab[2])
  props <- -diff(c(1, g))

  data.frame(de_grade = c(1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4,5),
             prop = props)
}

# note: works on percentages
# straight from the excel sheets

everygrade <- sapply(0:100, degrader)



# which beta distribution should we treat as ideal?
ff <- function(ab) { sum((qbeta(p = c(0.05, 0.5, 0.95),
                                ab[1], ab[2]) - c(.70, .85, .95))^2) }
optim(ab, fn = ff)
# 18.2, 3.4

g <- runif(24, min = 0.3, max=1)
p <- 0.85

# for the screwed up ssheet

grabcomma <- function(){
  read.csv(pipe("pbpaste"), sep = "\t", dec = ",",
           header = FALSE)[[1]]
}

grabpoint <- function(){
  read.csv(pipe("pbpaste"), sep = "\t", dec = ".",
           header = FALSE)[[1]]
}

# old hertie love
hertielove_old <- function(r, v, plot = FALSE){
  lg <- log(r) - log(1 - r)
  f <- function(alpha) (median(plogis(lg - alpha)) - v)^2
  alpha <- optimize(f, c(-5,5))$minimum
  res <- plogis(lg - alpha)

  message(sprintf("old: %.3f\nnew: %.3f\nalpha: %.3f\n",
                  mean(r), mean(res), alpha))
  cat(paste0(res, "\n", collapse = ""))
  if (plot) {
    plot(r, res, xlim = 0:1, ylim = 0:1)
    abline(a = 0, b = 1, col = "grey")
  }
  invisible(res)
}
