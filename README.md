## Trusted Mini-Agents Demonstration

This repository contains the materials shared during the 2026 R/Pharma Summit during posit:conf 2026:

* Slides available at <https://rpodcast.github.io/tma-demo-rph2026/slides.html>
* Example Shiny application performing a basic clinical trial simulation using the trusted mini-agents workflow. This application is copied from the upstream trusted mini-agents guide available at <https://trustedminiagents.dev>.

## Development Setup

If you wish to run the Shiny application on your system, you can do so using the following procedure:

* Install the Nix Package Manager using the Determinant Systems installer for your operating system: <https://determinate.systems/posts/determinate-nix-installer>
* Clone this repository to your system.
* Navigate to the root of this repository and build the Nix environment by running `nix-build` in a terminal.
* Update the `new_chat` function located in the `R/tools.R` script to use your preferred AI provider supported by `{ellmber}`. In this repository, the Open Router service is used for the AI provider. You will need to create a new `.Renviron` file in the root of this repository. An example is provided in the `.Renviron.example` file.
