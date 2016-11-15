set.seed(42)
results <- replicate (100, {
n <- 1500
E <- rep(0, n)
E[sample(n, floor(n / 2))] <- 1

principle_strata <-
  sample(c("always taker",
    "never taker",
    "complier"), n,
    replace = TRUE,
    prob = c(1/3, 1/3, 1/3))
X1 <- rep(0, n)
X1[principle_strata %in% c("always taker", "complier")] <- 1
X0 <- rep(0, n)
X0[principle_strata %in% c("always taker")] <- 1
Y1 <- rbinom(n, 1, 0.6)
Y0 <- rbinom(n, 1, 0.4)

X <- E * X1 + (1 - E) * X0

Y <- X * Y1 + (1 - X) * Y0

numerator <-
  mean(Y[E == 1]) - mean(Y[E == 0])
denominator <-
  mean(X[E == 1]) - mean(X[E == 0])
wald <- numerator / denominator
wald
})
summary(results)
