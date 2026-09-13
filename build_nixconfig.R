library(rix)
rix(
  r_ver = "4.6.1",
  r_pkgs = c(
    "ellmer",
    "shinychat",
    "shiny",
    "devtools",
    "shinytest2",
    "bslib",
    "testthat",
    "reactable",
    "watcher"
  ),
  ide = "none",
  system_pkgs = c("quarto"),
  project_path = getwd(),
  overwrite = TRUE
)
