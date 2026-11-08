# Chapter 2 - An asset selling problem
# Working through the chapter example with real numbers.
# Run section by section (Cmd+Enter in RStudio / Positron), or source the whole file.

# =============================================================================
# Step 1: The five elements of the model (sections 2.4.1 - 2.4.5)
# =============================================================================
#
#   State        S_t = (R_t^asset, p_t)      R = 1 holding, 0 sold; p = price
#   Decision     x_t in {0, 1}               1 = sell, subject to x_t <= R_t
#   Exogenous    W_{t+1} = p_hat_{t+1}       the price change
#   Transition   R_{t+1} = R_t - x_t         (2.1)
#                p_{t+1} = p_t + p_hat_{t+1} (2.2)
#   Objective    C(S_t, x_t) = p_t * x_t     we earn the price once, when we sell
#
# Each element becomes a small R function so the code mirrors the math.

# Contribution C(S_t, x_t) = p_t * x_t
contribution <- function(state, x) {
  state$price * x
}

# Transition S_{t+1} = S^M(S_t, x_t, W_{t+1})
transition <- function(state, x, W) {
  stopifnot(x <= state$R) # can't sell what we don't hold
  list(
    R = state$R - x, # (2.1)
    price = state$price + W # (2.2)
  )
}

S0 <- list(R = 1, price = 45)
S0
transition(S0, x = 0, W = 1.25) # hold: still own it, price moves to 46.25
transition(S0, x = 1, W = 1.25) # sell: R drops to 0
contribution(S0, x = 1) # selling at t = 0 earns $45

# =============================================================================
# Step 2: Modeling uncertainty - Table 2.1 (section 2.5)
# =============================================================================
#
# The book's Excel trick: p_hat = Norm.inv(Rand(), 0, sigma).
# In R, Norm.inv is qnorm() (the inverse normal CDF) and Rand() is runif().
# Push uniform U through qnorm and you get a normal draw.

U <- c(
  0.8287,
  0.6257,
  0.9343,
  0.4879,
  0.3736,
  0.8145,
  0.0385,
  0.0089,
  0.9430,
  0.3693
)

table_2_1 <- data.frame(U = U, p_hat = round(qnorm(U, mean = 0, sd = 1), 4))
table_2_1 # matches Table 2.1 to ~3 decimals (the book printed U rounded to 4 places)

# Same thing with fresh random numbers (this is what rnorm() does for you)
set.seed(2)
qnorm(runif(5), mean = 0, sd = 1)

# =============================================================================
# Step 3: Sample paths (Table 2.2) and the sell-low policy (eq. 2.14)
# =============================================================================
#
# Each row is one sample path omega^n: a full sequence of prices p_1 .. p_8.
# The policy walks along a row one period at a time and never peeks ahead.

prices <- matrix(
  c(
    42.67,
    45.53,
    47.07,
    47.56,
    47.80,
    48.43,
    46.93,
    46.57,
    46.35,
    43.15,
    42.51,
    40.51,
    41.50,
    41.00,
    39.16,
    41.11,
    43.17,
    45.16,
    45.37,
    44.30,
    45.35,
    47.23,
    47.35,
    46.30,
    45.24,
    45.67,
    46.18,
    46.22,
    45.69,
    44.24,
    43.77,
    43.57,
    47.68,
    46.32,
    46.14,
    41.53,
    44.84,
    45.17,
    44.92,
    46.09,
    47.83,
    44.70,
    43.05,
    43.77,
    42.61,
    44.32,
    44.16,
    45.29,
    45.11,
    43.67,
    43.14,
    44.78,
    43.12,
    42.36,
    41.60,
    40.83,
    46.78,
    44.98,
    44.53,
    45.42,
    46.43,
    47.67,
    43.68,
    49.03,
    43.16,
    44.57,
    45.99,
    47.38,
    45.51,
    46.27,
    46.02,
    45.09,
    46.57,
    45.01,
    46.73,
    42.08,
    47.40,
    49.14,
    49.03,
    48.74
  ),
  nrow = 10,
  byrow = TRUE,
  dimnames = list(paste0("w", 1:10), paste0("t", 1:8))
)
prices
T_end <- ncol(prices) # T = 8: if we still hold the asset here, we must sell

# Sell-low policy X^{sell-low}(S_t | theta_low), eq. (2.14):
#   1 if p_t < theta_low,  1 if t = T,  0 otherwise
# It only needs the current state (R, price) plus the clock t and theta.
sell_low <- function(state, t, theta_low) {
  if (state$R == 0) {
    return(0)
  } # nothing left to sell
  if (state$price < theta_low) {
    return(1)
  }
  if (t == T_end) {
    return(1)
  } # forced sale at the end of the horizon
  0
}

# Initial state S_1: we hold the asset at the first observed price
initial_state <- function(p1) {
  list(R = 1, price = p1)
}

# Simulate one policy along one sample path, returning F_hat(omega).
# Note how it is just the loop  S_t -> x_t = X(S_t) -> W_{t+1} -> S_{t+1}.
simulate_path <- function(path, policy, theta, verbose = FALSE) {
  path <- unname(path)
  state <- initial_state(path[1])
  F_hat <- 0
  for (t in 1:T_end) {
    x <- policy(state, t, theta)
    F_hat <- F_hat + contribution(state, x)
    if (verbose) {
      cat(sprintf("t=%d  price=%.2f  R=%d  x=%d\n", t, state$price, state$R, x))
    }
    if (t < T_end) {
      W <- path[t + 1] - path[t] # p_hat_{t+1}, the observed price change
      state <- transition(state, x, W)
    }
  }
  F_hat
}

# The book's worked example: theta_low = $42 on path omega^5 -> $41.53
simulate_path(prices["w5", ], sell_low, theta = 42, verbose = TRUE)
simulate_path(prices["w2", ], sell_low, theta = 42, verbose = TRUE)
simulate_path(prices["w2", ], sell_low, theta = 38, verbose = TRUE)

# =============================================================================
# Step 4: Policy evaluation over all N sample paths (section 2.7)
# =============================================================================
#
#   F_bar^pi          = (1/N) sum_n F_hat^pi(omega^n)              (2.8)
#   (sigma_hat^pi)^2  = 1/(N-1) sum_n (F_hat^pi(omega^n) - F_bar)^2   spread of ONE path's outcome
#   (sigma_bar^pi)^2  = (1/N) (sigma_hat^pi)^2                        uncertainty in the AVERAGE

# Run the policy on every row (sample path) of the price matrix -> vector of F_hats
F_hats <- function(prices, policy, theta) {
  apply(prices, 1, simulate_path, policy = policy, theta = theta)
}

evaluate_policy <- function(prices, policy, theta) {
  F_hat <- F_hats(prices, policy, theta)
  N <- length(F_hat)
  F_bar <- mean(F_hat)
  sigma_hat2 <- sum((F_hat - F_bar)^2) / (N - 1) # same as var(F_hat)
  sigma_bar2 <- sigma_hat2 / N
  c(
    F_bar = F_bar,
    sigma_hat2 = sigma_hat2,
    sigma_bar2 = sigma_bar2,
    std_error = sqrt(sigma_bar2)
  )
}

# The book's theta_low = $42, on all 10 paths
F_hats(prices, sell_low, theta = 42)
evaluate_policy(prices, sell_low, theta = 42)

# Same policy class, different theta = a different policy pi
sapply(c(40, 42, 44, 46), function(th) evaluate_policy(prices, sell_low, th))

# Exercise 2.5 is now one line away -- left for you to run.

# =============================================================================
# Step 5a: More policy classes - high-low (2.15) and tracking (2.17)
# =============================================================================

# High-low: theta = c(low, high). Sell on a big drop OR a big jump.
high_low <- function(state, t, theta) {
  if (state$R == 0) {
    return(0)
  }
  if (state$price < theta["low"] || state$price > theta["high"]) {
    return(1)
  }
  if (t == T_end) {
    return(1)
  }
  0
}

# Tracking: sell when the price rises theta above its smoothed value p_bar.
# The policy needs p_bar_t, so p_bar_t must be in the state:
#   S_t = (R_t, p_t, p_bar_t)
# That means we "tweak the model": a new initial state and a new transition.
#
# Smoothing (2.16): p_bar_t = (1 - alpha) p_bar_{t-1} + alpha p_t
# (the book prints p_hat_t here, but smoothing the price p_t is what makes
#  sense for a "tracking signal" -- we smooth prices, not price changes)
alpha <- 0.3

initial_state <- function(p1) {
  list(R = 1, price = p1, p_bar = p1)
}

transition <- function(state, x, W) {
  stopifnot(x <= state$R)
  new_price <- state$price + W
  list(
    R     = state$R - x,                                 # (2.1)
    price = new_price,                                   # (2.2)
    p_bar = (1 - alpha) * state$p_bar + alpha * new_price # (2.16)
  )
}

track <- function(state, t, theta) {
  if (state$R == 0) {
    return(0)
  }
  if (state$price >= state$p_bar + theta) {
    return(1)
  }
  if (t == T_end) {
    return(1)
  }
  0
}

# The old policies still work: they just ignore p_bar
simulate_path(prices["w5", ], sell_low, theta = 42)

# Watch the tracking signal on omega^1 (a path that rises early)
simulate_path(prices["w1", ], track, theta = 1, verbose = TRUE)

# The three classes F = {sell-low, high-low, track}, each with a theta
evaluate_policy(prices, sell_low, theta = 42)
evaluate_policy(prices, high_low, theta = c(low = 42, high = 47))
evaluate_policy(prices, track, theta = 1)

# =============================================================================
# Step 5b: Comparing two policies - independent vs. same sample paths
# =============================================================================
#
# Is high-low really better than sell-low, or did it get lucky?
# We need more paths than Table 2.2 gives us, so we simulate them from the
# basic price model (2.13): p_{t+1} = p_t + p_hat, p_hat ~ N(0, sigma^2).

sigma <- sd(diff(t(prices))) # estimate sigma from the price changes in Table 2.2
sigma

simulate_prices <- function(N, p1 = 45, sigma) {
  p_hat <- matrix(rnorm(N * (T_end - 1), mean = 0, sd = sigma), nrow = N)
  cbind(p1, p1 + t(apply(p_hat, 1, cumsum))) # each row: p_1, p_1 + cumsum(changes)
}

z <- 1.645 # z_{.05}: P(Z >= 1.645) = .05  -> a 90% two-sided interval
N <- 500
theta_A <- c(low = 42, high = 47) # policy A: high-low
theta_B <- 42                     # policy B: sell-low

# --- Option 1: independent samples (each policy gets its OWN paths) ---------
set.seed(10)
paths_A <- simulate_prices(N, sigma = sigma)
paths_B <- simulate_prices(N, sigma = sigma)

eval_A <- evaluate_policy(paths_A, high_low, theta_A)
eval_B <- evaluate_policy(paths_B, sell_low, theta_B)

diff_indep <- eval_A[["F_bar"]] - eval_B[["F_bar"]]
se_indep <- sqrt(eval_A[["sigma_bar2"]] + eval_B[["sigma_bar2"]])
c(diff = diff_indep, lower = diff_indep - z * se_indep, upper = diff_indep + z * se_indep)

# --- Option 2: same samples (both policies on the SAME paths) ---------------
# delta_F_hat(omega) = F_hat^A(omega) - F_hat^B(omega), then treat the deltas
# as a single sample. Shared luck cancels out in the difference.
paths <- paths_A
delta <- F_hats(paths, high_low, theta_A) - F_hats(paths, sell_low, theta_B)

diff_paired <- mean(delta)
se_paired <- sqrt(var(delta) / N)
c(diff = diff_paired, lower = diff_paired - z * se_paired, upper = diff_paired + z * se_paired)

# How much tighter is the paired interval?
c(se_independent = se_indep, se_paired = se_paired, ratio = se_indep / se_paired)

# =============================================================================
# Step 6a: Time series price process (section 2.8.1)
# =============================================================================
#
#   p_{t+1} = eta0 p_t + eta1 p_{t-1} + eta2 p_{t-2} + eps_{t+1}     (2.18)
#
# To simulate the next price we now need the last THREE prices, so they all
# go in the state: S_t = (R_t, p_t, p_{t-1}, p_{t-2}).

simulate_ar_prices <- function(n_periods, eta, p_init, sd_eps) {
  k <- length(eta) # number of lags
  p <- c(p_init, numeric(n_periods - length(p_init)))
  for (t in k:(n_periods - 1)) {
    lags <- p[t:(t - k + 1)] # (p_t, p_{t-1}, ..., p_{t-k+1}) -- this is the price part of S_t
    p[t + 1] <- sum(eta * lags) + rnorm(1, 0, sd_eps)
  }
  p
}

set.seed(26)
p_ar <- simulate_ar_prices(20, eta = c(0.7, 0.2, 0.1), p_init = c(45, 44, 46), sd_eps = 1)
round(p_ar, 2)

# =============================================================================
# Step 6b: Learning eta with recursive least squares (section 2.8.2)
# =============================================================================
#
# Now eta is unknown. We hold an estimate eta_bar_t and update it each time a
# new price arrives, without refitting a regression from scratch:
#
#   phi_t     = (p_t, p_{t-1})                         explanatory variables
#   p_bar_t   = eta_bar_t' phi_t                       forecast of p_{t+1}
#   eps_hat   = p_bar_t - p_{t+1}                      (2.20) forecast error
#   gamma_t   = 1 + phi_t' M_{t-1} phi_t               (2.22)
#   M_t       = M_{t-1} - M_{t-1} phi phi' M_{t-1} / gamma_t   (2.21)
#   eta_bar_{t+1} = eta_bar_t - M_{t-1} phi_t eps_hat / gamma_t  (2.23, see note)
#
# NOTE on (2.23): the book prints (1/gamma_t) M_t, but M_t phi_t already equals
# M_{t-1} phi_t / gamma_t, so that would divide by gamma twice. The standard
# RLS update uses M_{t-1} / gamma_t (equivalently M_t with no 1/gamma).
# We check this against lm() below rather than taking anyone's word for it.

rls_update <- function(eta_bar, M, phi, p_next) {
  p_forecast <- sum(eta_bar * phi)
  eps_hat <- p_forecast - p_next                         # (2.20)
  gamma <- drop(1 + t(phi) %*% M %*% phi)                # (2.22)
  M_new <- M - (M %*% phi %*% t(phi) %*% M) / gamma      # (2.21)
  eta_new <- eta_bar - drop(M %*% phi) * eps_hat / gamma # (2.23), using M_{t-1}
  list(eta_bar = eta_new, M = M_new, eps_hat = eps_hat, gamma = gamma)
}

# Simulate a long price series with a TRUE eta that the learner doesn't know
eta_true <- c(0.6, 0.35)
set.seed(7)
p_learn <- simulate_ar_prices(300, eta = eta_true, p_init = c(45, 44), sd_eps = 4) # eps ~ N(0, 4^2) as in (2.19)

# Start from a vague guess: eta_bar = (0, 0), M huge (= "we know nothing")
eta_bar <- c(0, 0)
M <- diag(1e6, 2)
history <- matrix(NA, nrow = length(p_learn), ncol = 2, dimnames = list(NULL, c("eta0", "eta1")))

for (t in 2:(length(p_learn) - 1)) {
  phi <- c(p_learn[t], p_learn[t - 1])
  upd <- rls_update(eta_bar, M, phi, p_next = p_learn[t + 1])
  eta_bar <- upd$eta_bar
  M <- upd$M
  history[t + 1, ] <- eta_bar
}

# Watch the estimate converge toward eta_true
history[c(4, 5, 10, 25, 50, 100, 300), ]
eta_true

# Sanity check: RLS should match ordinary least squares on all the data
n <- length(p_learn)
fit <- lm(p_learn[3:n] ~ 0 + p_learn[2:(n - 1)] + p_learn[1:(n - 2)])
rbind(rls = eta_bar, lm = unname(coef(fit)))

# One single update, by hand, so you can see every number
eta_bar_t <- c(0.5, 0.4)
M_prev <- diag(0.01, 2)
phi_t <- c(30, 28)
step <- rls_update(eta_bar_t, M_prev, phi_t, p_next = 29)
sum(eta_bar_t * phi_t) # forecast p_bar_t = 0.5*30 + 0.4*28 = 26.2
step               # eps_hat = 26.2 - 29 = -2.8 (we under-forecast) -> both eta's move UP
