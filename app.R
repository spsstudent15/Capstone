# New York City Civil Service Jobs
# by Armenoush Aslanian-Persico

library(shiny)
library(plotly)

#dependencies
df <- read.csv("sal-title-year-comparison-long.csv")
dft <- read.csv("titledescriptions.csv")
dfa <- read.csv("sal-agency-year-comparison-long.csv")

#############################################

ui <-   
  
navbarPage("New York City Civil Service Jobs", id="nav", 


tabPanel("Introduction",
img(src="http://www1.nyc.gov/assets/designcommission/images/content/slideshows/1970-nyc-photo.jpg",
width = 400),    
p("p creates a paragraph of text."),
br(),
code("code displays your text similar to computer code"),
br()), 

      
tabPanel("Job Titles", 
  fluidPage(
  headerPanel('Hiring and Salaries by Title - Most Common Titles Only'),
  sidebarPanel(
    p("Select a civil service title below to see the average salary and count of jobs by year. 
      This is a select list of titles represented by the most agencies."),
    selectInput('Description', 'Civil Service Title', unique(df$Description), selected='ACCOUNTANT'),
    p("Summary statistics for the salary of the selected title:"),
    verbatimTextOutput('stats'),
    p("Description of the selected title:"),
    verbatimTextOutput('describe1'),

    tags$h6("The following titles are represented. 
            These are the 20 titles that are most common across all city agencies, 
            listed here in order of most common to least common.
            This select group of titles was chosen to normalize the data across agencies."),
    
    tags$h6("COMMUNITY ASSOCIATE,
  ADMINISTRATIVE STAFF ANALYST,
  COMMUNITY COORDINATOR,
  PRINCIPAL ADMINISTRATIVE ASSOC,
  CLERICAL ASSOCIATE,
  COMPUTER SYSTEMS MANAGER,
  EXECUTIVE AGENCY COUNSEL,
  ADM MANAGER-NON-MGRL FRM M1/M2,
  ASSOCIATE STAFF ANALYST,
  COMMUNITY ASSISTANT,
  SECRETARY,
  ADMINISTRATIVE PUBLIC INFORMAT,
  AGENCY ATTORNEY,
  COMPUTER SPECIALIST (SOFTWARE),
  STAFF ANALYST,
  ADMINISTRATIVE MANAGER,
  COMPUTER ASSOC (SOFTWARE),
  PROCUREMENT ANALYST,
  COMPUTER ASSOC (OPERATIONS),
  ACCOUNTANT
"),
    tags$h5("Source: NYC Open Data")
  ),
  mainPanel(
    fluidRow(splitLayout(cellWidths = c("50%", "50%"),
    plotlyOutput('plot1'),
    plotlyOutput('plot2') )
  )
))),

tabPanel("Agencies", 
         fluidPage(
           headerPanel('Hiring and Salaries by Agency  - Most Common Titles Only'),
           sidebarPanel(
             p("Select an agency below to see the average salary and count of jobs by year."),
             selectInput('agency', 'Agency Name', unique(dfa$agency), 
                         selected='ADMINISTRATION FOR CHILDRE'),
             tags$h6("The following titles are represented. 
            These are the 20 titles that are most common across all city agencies, 
            listed here in order of most common to least common.
            This select group of titles was chosen to normalize the data across agencies."),
             
             tags$h6("COMMUNITY ASSOCIATE,
  ADMINISTRATIVE STAFF ANALYST,
  COMMUNITY COORDINATOR,
  PRINCIPAL ADMINISTRATIVE ASSOC,
  CLERICAL ASSOCIATE,
  COMPUTER SYSTEMS MANAGER,
  EXECUTIVE AGENCY COUNSEL,
  ADM MANAGER-NON-MGRL FRM M1/M2,
  ASSOCIATE STAFF ANALYST,
  COMMUNITY ASSISTANT,
  SECRETARY,
  ADMINISTRATIVE PUBLIC INFORMAT,
  AGENCY ATTORNEY,
  COMPUTER SPECIALIST (SOFTWARE),
  STAFF ANALYST,
  ADMINISTRATIVE MANAGER,
  COMPUTER ASSOC (SOFTWARE),
  PROCUREMENT ANALYST,
  COMPUTER ASSOC (OPERATIONS),
  ACCOUNTANT
"),
             
             
             tags$h5("Source: NYC Open Data")
             ),
           mainPanel(
             fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                  plotlyOutput('plot3'),
                                  plotlyOutput('plot4') )
             )
           ))),



tabPanel("Insights",
         img(src="http://www1.nyc.gov/assets/designcommission/images/content/slideshows/1970-nyc-photo.jpg", width = 400),    
         p("p creates a paragraph of text."),
         br(),
         br(),
         code("code displays your text similar to computer code"),
         br()), 

tabPanel("Sources") 
)


#############################################

# Define server logic required to draw a scatterplot
server <- function(input, output, session) {
  
  #titles - locate data from salary dataframe (df)
  selectedData <- reactive({
    dfSlice <- df %>%
      filter(Description==input$Description)
  })
  
  #titles - locate data from title description dataframe (dft)
  selectedData2 <- reactive({
    dfSlice2 <- dft %>%
      filter(Title==input$Description)
  })
  
  #agency - locate data from salary dataframe (dfa)
  selectedData3 <- reactive({
    dfaSlice <- dfa %>%
      filter(agency==input$agency)
  })

  #titles plot 1  
  output$plot1 <- renderPlotly({
    dfSlice <- df %>%
      filter(Description == input$Description)
    x <- list(
      title = "Year"
    )
    y <- list(
      title = "Average Salary"
    )
   p<- plot_ly(selectedData(), 
            x = ~as.factor(year), 
            y = ~sal, 
            color = ~Description, 
            type='scatter',
            mode = 'lines',
            height = 350,
            width = 350) %>%
     layout(xaxis = x, yaxis = y, title = paste(input$Description) )
  })
  
#titles plot 2
  output$plot2 <- renderPlotly({
    dfSlice <- df %>%
      filter(Description == input$Description)
    x <- list(
      title = "Year"
    )
    y <- list(
      title = "Count of Jobs"
    )
    p<- plot_ly(selectedData(), 
                x = ~as.factor(year), 
                y = ~Count, 
                color = ~Description, 
                type='scatter',
                mode = 'lines',
                height = 350,
                width = 350) %>%
      layout(xaxis = x, yaxis = y, title = paste(input$Description) )
  })
  

#agency plot 1  
  output$plot3 <- renderPlotly({
    dfaSlice <- dfa %>%
      filter(agency == input$agency)
    x <- list(
      title = "Year"
    )
    y <- list(
      title = "Average Salary"
    )
    p<- plot_ly(selectedData3(), 
                x = ~as.factor(year), 
                y = ~sal, 
                color = ~agency, 
                type='scatter',
                mode = 'lines',
                height = 350,
                width = 350) %>%
      layout(xaxis = x, yaxis = y, title = paste(input$agency) )
  })
  
  
  
#agency plot 2  
  output$plot4 <- renderPlotly({
    dfaSlice <- dfa %>%
      filter(agency == input$agency)
    x <- list(
      title = "Year"
    )
    y <- list(
      title = "Count of Jobs"
    )
    p<- plot_ly(selectedData3(), 
                x = ~as.factor(year), 
                y = ~Count, 
                color = ~agency, 
                type='scatter',
                mode = 'lines',
                height = 350,
                width = 350) %>%
      layout(xaxis = x, yaxis = y, title = paste(input$agency) )
  })
    
# show statistics summary in box
  output$stats <- renderPrint({
    dfSliceName <- selectedData() %>%
      filter(Description == input$Description)
    summary(dfSliceName$sal)
  })

# show text description of title in box  
  output$describe1<- renderPrint({
    dfSliceName2 <- selectedData2() %>%
      filter(Title == input$Description)
    paste(dfSliceName2$Detail)
  })
    
  
# show text description of agency in box  
#  output$describe2<- renderPrint({
#    dfSliceName3 <- selectedData3() %>%
#      filter(Agency == input$agency)
#    paste(dfSliceName3$Detail)
#  })
  
  
}

#############################################

# Run the application 
shinyApp(ui = ui, server = server)

