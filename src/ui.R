#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Whatsapp Chat Insights"),

    # Sidebar with a slider input for number of bins
    tabsetPanel(
      id = "main_tab",
      type = "hidden",
      tabPanel("Page 1",
               fileInput("data",
                         "Upload the exported TXT file from Whatsapp",
                         accept = ".txt",
                         buttonLabel = "Browse",
                         placeholder = "No file selected")
        ),

        # Show a plot of the generated distribution
      tabPanel("Page 2",
        mainPanel(
            textOutput("confirmation"),
            textOutput("image_count"),
            plotOutput("user_message_chart"),
            plotOutput("most_used_words_text"),
            plotOutput("time_chart"),
            plotOutput("date_chart"),
            plotOutput("day_of_month_chart")
        )
      )
    )
)
