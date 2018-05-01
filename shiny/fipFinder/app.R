# install.packages("shiny")
# install.packages("rsconnect")
# install.packages("shinythemes")
library(shiny)
library(dplyr)
library(DT)
library(caret)
library(randomForest)
library(shinythemes)


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


# Now, for the ggplot on another tab
countFree_combi <- read.csv("plotData.csv")
countFree_combi$Day <- factor(countFree_combi$Day, levels = countFree_combi$Day)

# For the next stats
arngd_combi <- read.csv("freeCountTable.csv")
colnames(arngd_combi)[2] <- "Number of days IP found free"

topFree <- head(arngd_combi, 10)
topActive <- tail(arngd_combi, 10) %>% arrange(`Number of days IP found free`)


# Define UI for application that draws a histogram
ui <- fluidPage(
    #themeSelector(),
    theme = shinytheme("simplex"),
   
   # Application title
   titlePanel("Free IP Finder"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateInput(inputId = "myDate",
                  label = "Which date do you want the free IP addresses for?",
                  min = "2016-07-01"),     # roughly the date from which we started living in theVS
        
        hr(), 
        # br(), br(), br(), br(), br(), br(), br(),
        p("In the faraway land of Pantnagar, there is a hostel called V.S. Bhawan where 
          the B.Tech. final year lives. Everything is perfect, as if it's the 
          second paradise on earth (the first being North Korea), except for one
          thing: frequent IP conflicts. The number of IP addresses are limited on the
          WLAN and there are frequent cases of IP being stolen on a daily basis.
          Many are frustrated by manually searching for free IPs repeatedly. Some of them are 
          even considering increasing their personal monthly mobile data plan 
          instead of relying on the Wi-Fi.
          "),
        p("
          This Free IP Finder app is an attempt to bring justice to all those
          who have the rightful ownership of the IP addresses and deserves a 
          free, fast and stable internet. 
          "),
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
        tabsetPanel(id = "tabspanel", type = "tabs",
                    tabPanel(title = "Free IP",
                              h3(uiOutput(outputId = "phrase")),  
                              dataTableOutput(outputId = "IPList")
                    ),
                    tabPanel(title = "Stats",
                             h3(uiOutput(outputId = "another_phrase")),
                             br(),
                             plotOutput(outputId = "weeklyPlot"),
                             p("From the above plot, it's clearly visible that less number of IP 
                               addresses are available as free in the first 3 days of the week as 
                               compared to the second half of the week."),
                             br(),
                             h3("Top 10 IP Addresses that are most frequently found free and that are found
                                occupied"), br(),
                             p("Below stats correspond to 66 days of daily gathered data."),
                             fluidRow(
                               column(6,
                                      h4("Top 10 free IPs"), br(),
                                      tableOutput(outputId = "topFree")
                               ),
                               column(6,
                                      h4("Top 10 occupied IPs"), br(),
                                      tableOutput(outputId = "topActive")
                               )
                             )
                    )
        )
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
  
  # got this idea from DataCamp, used only for the "select" argument
  observeEvent(input$myDate, {
    if(input$myDate){
      showTab(inputId = "tabspanel", target = "Free IP", select = TRUE)
    }
  })

  # new TAB for ggplot starts from here
  output$another_phrase <- renderUI({
    HTML("The pattern of average number of free IP addresses on different days (out of 254)")
  })
  
  output$weeklyPlot <- renderPlot({
    ggplot(countFree_combi, aes(x = Day, y = count, fill = Day)) + 
      geom_bar(stat = "identity", alpha = 0.5) + 
      scale_y_continuous(limits = c(0, 254), breaks = c(pretty(1:200), 254)) +
      geom_text(aes(label = count), vjust = 0, size = 5)
  })
  
  output$topFree <- renderTable(topFree)
  output$topActive <- renderTable(topActive)

}

# Run the application 
shinyApp(ui = ui, server = server)

# Basic working App built on 10-04-18!

# Formally wrapped up on 13-04-18.12:33AM after adding a lot other features.
# But certainly, will revisit it again. 