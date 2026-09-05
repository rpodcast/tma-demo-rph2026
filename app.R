ui <- bslib::page_sidebar(
  title = "Clinical trial simulation agent",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  sidebar = bslib::sidebar(
    title = "AI chat (do not trust AI model output in here)",
    width = "30vw",
    style = "height: 100%; padding-top: 15px; overflow-x: hidden;",
    shinychat::chat_ui(
      "chat",
      messages = paste(
        "Describe clinical trial design scenarios, and my tools will simulate",
        "the probability of declaring efficacy for each scenario.",
        "I can simulate trials with continuous endpoints or binary endpoints.\n\n",
        "Example prompt:\n\n",
        "\"I am designing a trial with 100 patients per arm and a binary endpoint.",
        "My hypothesis test has a significance level of 0.05.",
        "Help me begin a simulation exercise by setting up two scenarios:",
        "one to simulate power under the assumption that",
        "treatment is better than control,",
        "and another to simulate the type I error",
        "assuming treatment and control are equally effective.",
        "In this first pass, make up ballpark response rates to",
        "represent these power and type I error scenarios.\""
      )
    )
  ),
  bslib::card(
    bslib::card_header("Design assumptions (HUMAN USER MUST REVIEW THIS)"),
    bslib::card_body(
      shiny::tableOutput("scenarios")
    )
  ),
  bslib::card(
    bslib::card_header("Results (trusted given the assumptions)"),
    bslib::card_body(
      shiny::tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  values <- shiny::reactiveValues(
    assumptions = NULL,
    results = NULL
  )
  delayedAssign(x = "chat", value = new_chat(values))
  shiny::observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input, stream = "content")
    shinychat::chat_append("chat", stream)
  })
  output$scenarios <- shiny::renderTable({
    req(values$assumptions)
    values$assumptions
  })
  output$results <- shiny::renderTable({
    req(values$results)
    values$results
  })
}

shiny::shinyApp(ui, server)
