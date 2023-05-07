
library(glue)
library(shiny)
source("src/utils.R")
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  file <- reactive({input$data})
  datapath <- reactive({input$data$datapath})
  extension <- reactive({tools::file_ext(datapath())})
  
  observeEvent(input$data, {
    updateTabsetPanel(session, "main_tab", selected = "Page 2")
  })
  
  req(file)
    output$confirmation <- renderText({
      print("validation")
      if(extension() != "txt"){
        "Please check and upload .txt file only"
      }
      else {
        "Upload Successful"
      }
    })
    
    df <- reactive({get_data(readLines(datapath()))})
    df_with_dates <- reactive({
      data <- df()
      data$date <- as.POSIXct(data$date, format = "%m/%d/%y, %H:%M")
      data
    })
    print("file read complete")
     
    output$image_count <- renderText({ 
      print("counting image")
      if(extension() == "txt"){
        #raw_text <- readLines(datapath())
        #df <- get_data(raw_text)
        image <- total_image(df())
        glue('Total Images shared is : {image}')
      }
    
    else {
        "Invalid File format"
    }
    })
    
  output$user_message_chart <- renderPlot({
    print("printing user chart")
    #adf <- get_data(readLines(datapath()))
    user_message_count(df())
  })

  output$most_used_words_text <- renderPlot({
    print("most used word")
    most_used_word(df())
  })
  
  output$time_chart <- renderPlot({
    most_active_time(df_with_dates())
  })
  
  output$date_chart <- renderPlot({
    most_active_date_of_month(df_with_dates())
  })
  
  output$day_of_month_chart <- renderPlot({
    most_active_day_of_week(df_with_dates())
  })
  
 
}
