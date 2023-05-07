library(shiny)

source("src/ui.R")
source("src/server.R")

shinyApp(ui = ui, server = server, options = list(port = 8080))
