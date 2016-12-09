set.seed(42)
n <- 1000

U <- rep(0, n)
U[sample(n, floor(n/2))] <- 1

X <- rep(0, n)
X[sample(n, floor(n/2))] <- 1

A <- rep(0, n)
prob_A1_U <- plogis(rnorm(n, (2 * U - 1), .25))
A[sample(n, floor(n/2), prob = prob_A1_U)] <- 1

M <- rep(0, n)
prob_M1_A <- plogis(rnorm(n, 3 * (2 * A - 1), .25))
M[sample(n, floor(n/2), prob = prob_M1_A)] <- 1

prob_Y1_X_U <- plogis(
  1 * (2 * M - 1) +
    1.5 * rnorm(n, .25 * (2 * U - 1), .1) +
    0.5 * rnorm(n, .25 * (2 * X - 1), .1))

prob_Y0_X_U <- plogis(
  -1 * (2 * M - 1) +
    -1.5 * rnorm(n, .25 * (2 * U - 1), .1) +
     0.5 * rnorm(n, .25 * (2 * X - 1), .1))

Y1 <- rbinom(n, 1, prob_Y1_X_U)
Y0 <- rbinom(n, 1, prob_Y0_X_U)

true_att <- mean(Y1[A == 1]) - mean(Y[A == 0])

naive_att <- mean(Y[A == 1]) - mean(Y[A == 0])

##### missing stuff

# best guess with no unobservables
prob_x1_a1 <- mean(X[A == 1])
prob_x0_a1 <- 1 - prob_x1_a1
prob_u1_a1_x1 <- mean(U[A == 1 & X == 1])
prob_u1_a1_x0 <- mean(U[A == 1 & X == 0])
prob_u0_a1_x1 <- 1 - prob_u1_a1_x1
prob_u0_a1_x0 <- 1 - prob_u1_a1_x0
E_y_a0_x1_u1 <- mean(Y[A == 0 & X == 1 & U == 1])
E_y_a0_x1_u0 <- mean(Y[A == 0 & X == 1 & U == 0])
E_y_a0_x0_u1 <- mean(Y[A == 0 & X == 0 & U == 1])
E_y_a0_x0_u0 <- mean(Y[A == 0 & X == 0 & U == 0])

##### missing stuff

# front door estimator
prob_m1_a0_x1 <- mean(M[A == 0 & X == 1])
prob_m0_a0_x1 <- 1 - prob_m1_a0_x1
prob_m1_a0_x0 <- mean(M[A == 0 & X == 0])
prob_m0_a0_x0 <- 1 - prob_m1_a0_x0
mu_0_a1_fd <-
  E_y_a1_m1_x1 * prob_m1_a0_x1 * prob_x1_a1 +
  E_y_a1_m0_x1 * prob_m0_a0_x1 * prob_x1_a1 +
  E_y_a1_m1_x0 * prob_m1_a0_x0 * prob_x0_a1 +
  E_y_a1_m0_x0 * prob_m0_a0_x0 * prob_x0_a1

front_door <- mu_1_a1 - mu_0_a1_fd

abs(true_att - naive_att) - abs(true_att - front_door)
