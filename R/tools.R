type_vector <- function(type) {
  ellmer::type_array(
    type,
    paste(
      "A vector defining multiple clinical trial",
      "design scenarios to simulate.",
      "Each element defines one scenario."
    )
  )
}

#' @title Binary endpoint trial simulation tool constructor.
#' @description Create a trial simulation tool for `ellmer` that estimates
#'   the probability of declaring efficacy in a two-arm RCT with a binary
#'   endpoint. The tool runs `simulate_trial_binary()` with the
#'   AI-supplied assumptions and writes the results to a Shiny
#'   `reactiveValues()` list.
#' @return An `ellmer` tool object that can be registered with
#'   an `ellmer` chat object.
#' @param values A Shiny `reactiveValues()` list. The tool writes two slots:
#'   `values$assumptions` (a one-row data frame of the assumptions used) and
#'   `values$efficacy_probability` (the Monte Carlo estimate). The tool is
#'   the ONLY thing that can update these reactive values. This restriction
#'   is essential for a trusted mini-agent.
new_binary_tool <- function(values) {
  ellmer::tool(
    fun = function(
      scenario,
      n_treatment,
      n_control,
      response_rate_treatment,
      response_rate_control,
      significance_level
    ) {
      assumptions <- data.frame(
        n_treatment = n_treatment,
        n_control = n_control,
        response_rate_treatment = response_rate_treatment,
        response_rate_control = response_rate_control,
        significance_level = significance_level
      )
      efficacy <- purrr::pmap_dbl(assumptions, simulate_trial_binary)
      results <- data.frame(scenario = scenario, efficacy = efficacy)
      values$assumptions <- cbind(scenario = scenario, assumptions)
      values$results <- results
      ellmer::ContentToolResult(
        paste(jsonlite::toJSON(results, digits = NA), collapse = "\n")
      )
    },
    name = "simulate_trial_binary",
    description = paste(
      "Simulate a parallel two-arm randomized controlled trial",
      "with a binary endpoint.",
      "Estimates the probability that the trial declares efficacy",
      "by sampling responders from independent binomial distributions",
      "and applying a one-sided test of proportions to each simulated trial.",
      "Use this tool when the user asks about power, type I error,",
      "or the probability of success for a trial with a binary outcome."
    ),
    arguments = list(
      scenario = ellmer::type_string(
        "Name of the clinical trial simulation scenario."
      ) |>
        type_vector(),
      n_treatment = ellmer::type_integer(
        "Number of patients in the treatment arm. Must be a positive integer."
      ) |>
        type_vector(),
      n_control = ellmer::type_integer(
        "Number of patients in the control arm. Must be a positive integer."
      ) |>
        type_vector(),
      response_rate_treatment = ellmer::type_number(
        paste(
          "True probability of a favorable response in the treatment arm.",
          "Must be between 0 and 1."
        )
      ) |>
        type_vector(),
      response_rate_control = ellmer::type_number(
        paste(
          "True probability of a favorable response in the control arm.",
          "Must be between 0 and 1."
        )
      ) |>
        type_vector(),
      significance_level = ellmer::type_number(
        paste(
          "One-sided significance threshold for declaring efficacy.",
          "Must be between 0 and 1. Typical value: 0.025."
        )
      ) |>
        type_vector()
    )
  )
}

#' @title Continuous endpoint trial simulation tool constructor.
#' @description Create a trial simulation tool for `ellmer` that estimates
#'   the probability of declaring efficacy in a two-arm RCT with a continuous
#'   endpoint. The tool runs `simulate_trial_continuous()` with the
#'   AI-supplied assumptions and writes the results to a Shiny
#'   `reactiveValues()` list.
#' @return An `ellmer` tool object that can be registered with
#'   an `ellmer` chat object.
#' @param values A Shiny `reactiveValues()` list. The tool writes two slots:
#'   `values$assumptions` (a one-row data frame of the assumptions used) and
#'   `values$efficacy_probability` (the Monte Carlo estimate). The tool is
#'   the ONLY thing that can update these reactive values. This restriction
#'   is essential for a trusted mini-agent.
new_continuous_tool <- function(values) {
  ellmer::tool(
    fun = function(
      scenario,
      n_treatment,
      n_control,
      mean_treatment,
      mean_control,
      sd_treatment,
      sd_control,
      significance_level
    ) {
      assumptions <- data.frame(
        n_treatment = n_treatment,
        n_control = n_control,
        mean_treatment = mean_treatment,
        mean_control = mean_control,
        sd_treatment = sd_treatment,
        sd_control = sd_control,
        significance_level = significance_level
      )
      efficacy <- purrr::pmap_dbl(assumptions, simulate_trial_continuous)
      results <- data.frame(scenario = scenario, efficacy = efficacy)
      values$assumptions <- cbind(scenario = scenario, assumptions)
      values$results <- results
      ellmer::ContentToolResult(
        paste(jsonlite::toJSON(results, digits = NA), collapse = "\n")
      )
    },
    name = "simulate_trial_continuous",
    description = paste(
      "Simulate a parallel two-arm randomized controlled trial",
      "with a continuous endpoint.",
      "Estimates the probability that the trial declares efficacy",
      "by sampling patient responses from independent normal distributions",
      "and applying a one-sided two-sample t-test to each simulated trial.",
      "Use this tool when the user asks about power, type I error,",
      "or the probability of success for a trial with a continuous outcome."
    ),
    arguments = list(
      scenario = ellmer::type_string(
        "Name of the clinical trial simulation scenario."
      ) |>
        type_vector(),
      n_treatment = ellmer::type_integer(
        "Number of patients in the treatment arm. Must be a positive integer."
      ) |>
        type_vector(),
      n_control = ellmer::type_integer(
        "Number of patients in the control arm. Must be a positive integer."
      ) |>
        type_vector(),
      mean_treatment = ellmer::type_number(
        "True mean response in the treatment arm."
      ) |>
        type_vector(),
      mean_control = ellmer::type_number(
        "True mean response in the control arm."
      ) |>
        type_vector(),
      sd_treatment = ellmer::type_number(
        "Standard deviation of responses in the treatment arm. Must be positive."
      ) |>
        type_vector(),
      sd_control = ellmer::type_number(
        "Standard deviation of responses in the control arm. Must be positive."
      ) |>
        type_vector(),
      significance_level = ellmer::type_number(
        paste(
          "One-sided significance threshold for declaring efficacy.",
          "Must be between 0 and 1. Typical value: 0.025."
        )
      ) |>
        type_vector()
    )
  )
}

new_chat <- function(values) {
  chat <- ellmer::chat_openrouter(
    system_prompt = paste(
      "You are a statistician and clinical trial expert.",
      "You have tools for simulating clinical trials",
      "with continuous and binary endpoints.",
      "When a user asks about the probability of success,",
      "power, or type I error for a trial,",
      "you use the tools to run simulations.",
      "Trust those tools to report the results."
    ),
    model = "~moonshotai/kimi-latest"
  )
  chat$register_tool(new_continuous_tool(values))
  chat$register_tool(new_binary_tool(values))
  chat
}
