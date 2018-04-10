# install.packages("shiny")
# install.packages("rsconnect")
library(shiny)
library(dplyr)
library(DT)
library(caret)
library(randomForest)


# reading my final RF model
rf_model <- readRDS("RF_final_model.rds")

# creating a basic data frame that will contain the new predictors based on user input
testSet <- data.frame(IpAddress = as.integer(c(1:254)),
                      Day = as.factor(NA),
                      DateOnly = as.integer(rep(0, 254)),
                      Probability = as.numeric(NA))

# Give proper factor levels to Day
levels(testSet$Day) <- c("Sunday", "Monday", "Tuesday", "Wednesday",
                         "Thursday", "Friday", "Saturday")

str(testSet)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Free IP Finder"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateInput(inputId = "myDate",
                  label = "Which date do you want the free IP addresses for?",
                  min = "2016-07-01"),     # roughly the date from which we started living in theVS
        
        br(), br(), br(), br(), br(), br(), br(),
        wellPanel(  
        h5("Developed by ", a("Vikas Dhyani", 
                              href = "https://www.linkedin.com/in/vikas-dhyani-792704114/")),
        h5(a(img(src = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg",
                 height = "15px"), 
             "GitHub",
             href = "https://github.com/vdhyani96")))
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
          h3(uiOutput(outputId = "phrase")),  
          dataTableOutput(outputId = "IPList")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  day <- reactive({ weekdays(input$myDate) })

  output$phrase <- renderUI({
    HTML(paste0("Likelihood of IP addresses to be free on ", input$myDate, 
                " (", day(), ") between 9 pm to 11 pm within the subnet of the V.S. Bhawan<br>"))
  })
  
  output$IPList <- renderDataTable({
    req(input$myDate)
    dateOnly <- format(input$myDate, "%d") %>% as.integer()
    
    testSet$Day[] <- day()  
    # This above took a really long time to figure out. From now, if want to replace all the values in 
    # a column, the above way is to be preferred, still with some caveat. 
    
    testSet$DateOnly[] <- dateOnly
    
    # Now, make predictions
    predProb <- predict(rf_model, testSet[, c(1:3)], type = "prob")
    testSet$Probability <- predProb$`1`
    
    datatable(
      data = arrange(testSet[, c(1, 4)], desc(Probability)), 
      options = list(pageLength = 10,
                     lengthMenu = c(10, 25, 40),
                     columnDefs = list(list(className = 'dt-center', targets = 0:1))),
      rownames = FALSE
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

# Basic working App built on 10-04-18!

