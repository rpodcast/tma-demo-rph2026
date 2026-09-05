#' @title Simulate a two-arm randomized controlled trial
#'   with a continuous endpoint.
#' @description Estimates the probability that a parallel two-arm RCT declares
#'   efficacy for a continuous outcome. Patient responses are sampled from
#'   independent normal distributions, and a one-sided two-sample t-test
#'   is applied to each simulated trial. The returned value is an
#'   estimate of the probability that the trial declares efficacy
#'   under the supplied assumptions.
#' @return Numeric scalar between 0 and 1,
#'   the proportion of simulated trials that declare efficacy.
#' @param n_treatment Integer, number of patients in the treatment arm.
#' @param n_control Integer, number of patients in the control arm.
#' @param mean_treatment Numeric, true mean response in the treatment arm.
#' @param mean_control Numeric, true mean response in the control arm.
#' @param sd_treatment Positive numeric, standard deviation of responses in
#'   the treatment arm.
#' @param sd_control Positive numeric, standard deviation of responses in
#'   the control arm.
#' @param significance_level Numeric between 0 and 1, one-sided significance
#'   threshold for declaring efficacy.
#' @examples
#'   simulate_trial_continuous()
simulate_trial_continuous <- function(
  n_treatment = 100,
  n_control = 100,
  mean_treatment = 0.5,
  mean_control = 0,
  sd_treatment = 1,
  sd_control = 1,
  significance_level = 0.025
) {
  set.seed(0L)
  stopifnot(is.numeric(n_treatment), n_treatment > 0)
  stopifnot(is.numeric(n_control), n_control > 0)
  stopifnot(is.numeric(mean_treatment))
  stopifnot(is.numeric(mean_control))
  stopifnot(is.numeric(sd_treatment), sd_treatment > 0)
  stopifnot(is.numeric(sd_control), sd_control > 0)
  stopifnot(
    is.numeric(significance_level),
    significance_level > 0,
    significance_level < 1
  )
  p_values <- replicate(10000, {
    treatment <- rnorm(n_treatment, mean_treatment, sd_treatment)
    control <- rnorm(n_control, mean_control, sd_control)
    test <- t.test(treatment, control, alternative = "greater")
    test$p.value
  })
  mean(p_values < significance_level)
}

#' @title Simulate a two-arm randomized controlled trial
#'   with a binary endpoint.
#' @description Estimates the probability that a parallel two-arm RCT declares
#'   efficacy for a binary outcome. The number of responders in each arm is
#'   sampled from independent binomial distributions, and a one-sided test of
#'   proportions is applied to each simulated trial. The returned value is an
#'   estimate of the probability that the trial declares efficacy
#'   under the supplied assumptions.
#' @return Numeric scalar between 0 and 1,
#'   the proportion of simulated trials that declare efficacy.
#' @param n_treatment Integer, number of patients in the treatment arm.
#' @param n_control Integer, number of patients in the control arm.
#' @param response_rate_treatment Numeric between 0 and 1, true probability
#'   of a favorable response in the treatment arm.
#' @param response_rate_control Numeric between 0 and 1, true probability
#'   of a favorable response in the control arm.
#' @param significance_level Numeric between 0 and 1, one-sided significance
#'   threshold for declaring efficacy.
#' @examples
#'   simulate_trial_binary()
simulate_trial_binary <- function(
  n_treatment = 100,
  n_control = 100,
  response_rate_treatment = 0.5,
  response_rate_control = 0.5,
  significance_level = 0.025
) {
  set.seed(0L)
  stopifnot(is.numeric(n_treatment), n_treatment > 0)
  stopifnot(is.numeric(n_control), n_control > 0)
  stopifnot(
    is.numeric(response_rate_treatment),
    response_rate_treatment >= 0,
    response_rate_treatment <= 1
  )
  stopifnot(
    is.numeric(response_rate_control),
    response_rate_control >= 0,
    response_rate_control <= 1
  )
  stopifnot(
    is.numeric(significance_level),
    significance_level > 0,
    significance_level < 1
  )
  p_values <- replicate(10000, {
    treatment <- rbinom(n_treatment, 1, response_rate_treatment)
    control <- rbinom(n_control, 1, response_rate_control)
    test <- prop.test(
      c(sum(treatment), sum(control)),
      c(n_treatment, n_control),
      alternative = "greater"
    )
    test$p.value
  })
  mean(p_values < significance_level)
}
