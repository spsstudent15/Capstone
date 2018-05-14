# New York City Civil Service Jobs
# by Armenoush Aslanian-Persico

library(dplyr)
library(shiny)
library(plotly)

#dependencies
df <- read.csv("sal-title-year-comparison-long.csv")
dft <- read.csv("titledescriptions.csv") 
dfa <- read.csv("sal-agency-year-comparison-long.csv")

dfsub1<- data.frame(read.csv("dfsub1.csv"),stringsAsFactors = FALSE)
dfsub17<- dfsub1[dfsub1$year==2017,]

#############################################

ui <-   
navbarPage("New York City Civil Service Jobs", id="nav", 
           
tabPanel("Introduction",
  fluidPage(
  sidebarPanel(
    img(src="http://www1.nyc.gov/assets/international/images/content/pages/unisphere_preview.jpg", width = "100%"),
    p("The Unisphere."), 
    p("Flushing Meadows Corona Park, Queens."),
    p("Photo from NYC.gov."), width=4),
  mainPanel(
    p(h1("New York City Civil Service Jobs")),
    p(h3("Armenoush Aslanian-Persico")),
    br(),br(),
    p("The City of New York has over 300,000 employees."),
    p("They serve in civil service titles, and each title has a required minimum and maximum salary."),
    p("Payroll and Title data are availble on the city's Open Data website. This data shows trends in hiring, salaries, and agency workforces.")
))), 


tabPanel("All Salaries",
         fluidPage(
           sidebarPanel(
             p("The displayed histogram shows all 2017 salaries with employees earning over $10,000."),
             p("The right tail consists primarily of managerial positions including Commissioners, senior agency directors, lawyers, and technical positions."),
             p("The spike in salaries between $85,000 and $95,000 is mostly comprised of police officers at the NYPD.")
             ),
           
           mainPanel(
             img(src="http://i.imgur.com/88kEwbW.jpg", width = "100%")
           ))), 

tabPanel("Influencers",
         fluidPage(
           #sidebarPanel(p("Agencies and titles with the greatest number of high paying jobs.")),
           mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot7"),(plotlyOutput("plot7a") ))))
         )),


tabPanel("Flexible Titles",
  plotlyOutput("plot8")),
      
tabPanel("Who's Earning?", 
  fluidPage(
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
  
tabPanel("Who's Hiring?", 
  fluidPage(
  sidebarPanel(
            p("Select an agency below to see the average salary and count of jobs by year."),
             selectInput('agency', 'Agency Name', unique(dfa$agency), 
                         selected='ADMINISTRATION FOR CHILDRE'),
             tags$h6("The following titles are represented. 
            These are the 20 titles that are most common across all city agencies, 
            listed here in order of most common to least common.
            This select group of titles was chosen to normalize the data across agencies."),
             tags$h6(
            "COMMUNITY ASSOCIATE,
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


tabPanel("Pay Raises",
          fluidPage(
            sidebarPanel(p("The displayed plot shows the greatest percentage salary increases out of the select group of flexible titles."),p(" 
                           Large jumps in salary or headcount are often caused by the scheduling of civil service exams and the establishment of exam lists. "),p(" 
                           An employee with a high exam score can be called from a list and receive a new position, potentially with a higher salary.")),
            mainPanel(plotlyOutput("plot6"))
          )),

tabPanel("Growing Agencies",
          fluidPage(
            sidebarPanel(p("The displayed plot shows the greatest percentage headcount increases across agencies for the select group of flexible titles."),
                         p("For the select group of 20 titles, three of the top 10 fastest growing agencies were District Attorneys' Offices. 
                           The largest by far was the Department of Correction, with an 86% headcount increase for these 20 titles combined.")),
            mainPanel(plotlyOutput("plot5"))
          )),

tabPanel("Promotions",
    fluidPage(
    sidebarPanel(p("The displayed plot shows ranges for select titles.  
                   Computer Systems Manager, a technical title requiring programming knowledge, 
                   has one of the broadest ranges with one of the highest average salaries."),p("
                   A person hired in this or a similarly broad-range title could have many 
                   opportunities for increases while remaining in the title.") ,p( "This also suggests 
                   that there are different skill and managerial levels within the same title, 
                   another indication of the title's flexibility.")),
    mainPanel(plotlyOutput("boxplot1"))
          )),

tabPanel("Insights",
         fluidPage(
           sidebarPanel(
             img(src="http://i.imgur.com/AOo6RSA.jpg", width = "100%"),
             p(),
             p("Freshkills Park, Staten Island."),
             p("Photo by the author.")),
           mainPanel(
             p(h2("Lessons From the Data")),
             p("The city sometimes takes years to update its datasets."),
             p("The payroll and titles datasets had errors and missing values which required significant cleanup."),
              p(h2("Lessons From the Project")),
              p("NYPD has the largest workforce by far."),
              p("The political influence of the Police, Fire, Correction, and Sanitation unions align with the workforce sizes."),
             p("The combined data can be very valuable to job seekers.")
             ))),


tabPanel("Sources",
         fluidPage(
           sidebarPanel(
             img(src="http://i.imgur.com/XkmtMLw.jpg", width = "100%"),
             p(),
             p("Mural of the 1928 Bayonne Bridge construction."),
            p("Staten Island Borough Hall. "),p("Photo by the author.")),
           mainPanel(
             p(h1("Sources")),
             br(),br(),
             a(href="https://data.cityofnewyork.us/City-Government/Civil-List/ye3c-m4ga", target="_blank", "NYC Payroll Dataset"),
             br(),br(),
             a(href="https://data.cityofnewyork.us/City-Government/NYC-Civil-Service-Titles/nzjr-3966", target="_blank", "NYC Titles Dataset"),
             br(),br(),
             a(href="http://www.nyc.gov/html/dcas/html/work/oldexams.shtml", target="_blank", "NYC Archive of Open Competitive Examinations"),
             br(),br(),
             a(href="https://www1.nyc.gov/site/doitt/initiatives/open-data-law.page", target="_blank", "NYC Open Data Law"),
             br(),br(),
             br(),
             a(href="https://github.com/spsstudent15/capstone", target="_blank", "Github Repository for This Project"),
             br(),br(),
             a(href="http://rpubs.com/aapsps/capstone-appendix-a", target="_blank", "Appendix A: Code and Output"),
             br(),br(),
             a(href="http://rpubs.com/aapsps/capstone-appendix-b", target="_blank", "Appendix B: Code and Output"),
             br(),br(),
             a(href="http://rpubs.com/aapsps/capstone-appendix-c", target="_blank", "Appendix C: Code and Output"),
             br(),br(),
             p(h5("Thanks for reading!"))
             ))) 
)


#############################################
#############################################
#############################################

# Define server logic
server <- function(input, output, session) {

#LOAD LISTS FOR DROP DOWNS
  #titles - locate data from salary dataframe (df)
  selectedData <- reactive({
    dfSlice <- df %>%
      filter(Description==input$Description)
  })
  
  #titles - locate data from title description dataframe (dft) #TO BE UPDATED
  selectedData2 <- reactive({
    dfSlice2 <- dft %>%
      filter(Title==input$Description)
  })
  
  #agency - locate data from salary dataframe (dfa)
  selectedData3 <- reactive({
    dfaSlice <- dfa %>%
      filter(agency==input$agency)
  })

# PLOT 1 - WHO'S EARNING
# titles plot 1  
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

# PLOT 2 - WHO'S EARNING
# titles plot 2
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
  
# PLOT 3 - WHO'S HIRING  
# agency plot 1  
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
  
  
# PLOT 4 - WHO'S HIRING 
# agency plot 2  
  output$plot4 <- renderPlotly({
    dfaSlice <- dfa %>% filter(agency == input$agency)
    x <- list(title = "Year")
    y <- list(title = "Count of Jobs")
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

# REACTIVE DROP DOWNS
# show statistics summary in box
  output$stats <- renderPrint({
    dfSliceName <- selectedData() %>%
      filter(Description == input$Description)
    summary(dfSliceName$sal)
  })

# REACTIVE DROP DOWNS
# show text description of title in box  
  output$describe1<- renderText({
    dfSliceName2 <- selectedData2() %>%
      filter(Title == input$Description)
    paste(dfSliceName2$Detail)
  })

# BOX PLOT - PROMOTIONS  
  output$boxplot1<- renderPlotly({print(plot_ly(dfsub17, y = ~sal, color = ~Description, type = "box", height = 600)
                                        %>%
                                          layout(title="Salary ranges, select titles", margin = list(b = 200), xaxis = list(tickangle = 90))
                                        )})
  
# show text description of agency in box  
#  output$describe2<- renderPrint({
#    dfSliceName3 <- selectedData3() %>%
#      filter(Agency == input$agency)
#    paste(dfSliceName3$Detail)
#  })

  #PLOT 5 - GROWING AGENCIES
  dfheadcount<- data.frame(read.csv("headcountbyagency-pct-increase-top10.csv"),stringsAsFactors = FALSE, strip.white=TRUE)
  dfheadcount$abbreviation <- factor(dfheadcount$abbreviation, levels = unique(dfheadcount$abbreviation)[order(dfheadcount$pctincrease, decreasing = TRUE)])
  
  output$plot5<- renderPlotly({print(
    plot_ly(data = dfheadcount, 
            x = ~abbreviation, 
            y = ~pctincrease, 
            type = 'bar', 
            hoverinfo='text',
            marker=list(color='#45B39D'),
            height = 600,
            text = ~paste(
              'Agency: ', agency,
              '<br> Headcount Percent Increase: ', pctincrease) ) %>%
      layout(title="Headcount increases 2014-2017, top 10 agencies, select titles", margin = list(b = 200), xaxis = list(tickangle = 90))
    
    )
  }) 
  
  #PLOT 6 - PAY RAISES
  dfraises<- data.frame(read.csv("bestraises-selecttitles.csv"),stringsAsFactors = FALSE, strip.white=TRUE)
  dfraises$title <- factor(dfraises$title, levels = unique(dfraises$title)[order(dfraises$pctincrease, decreasing = TRUE)])
 
  output$plot6<- renderPlotly({print(
    plot_ly(
      data = dfraises, 
      x = ~title, 
      y = ~pctincrease, 
      type = 'bar', 
      hoverinfo='text',
      marker=list(color='#82E0AA'),
      height = 600,
      text = ~paste(
        'Agency: ', title,
        '<br> Salary Percent Increase: ', pctincrease) ) %>%
      layout(title="Salary increases 2014-2017, top 10 titles, select titles", margin = list(b = 200), xaxis = list(tickangle = 90))
  ) }) 
  
  #PLOT 7 - INFLUENCERS - AGENCIES
  df3<- data.frame(read.csv("agency17high.csv"),stringsAsFactors = FALSE)
  df3 %>% mutate_if(is.factor, as.character) -> df3
  output$plot7<- renderPlotly({print(
    df3 %>%
      plot_ly(labels = ~ADDRESS, values = ~n) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Agencies with the most jobs over $10K",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    ) }) 
  
  #PLOT 7A - INFLUENCERS - TITLES
  df4<- data.frame(read.csv("agency17high-titles.csv"),stringsAsFactors = FALSE)
  
  df4 %>% mutate_if(is.factor, as.character) -> df4
  output$plot7a<- renderPlotly({print(
    df4 %>%
      plot_ly(labels = ~Description, values = ~n) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Titles with the most jobs over $10K",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(orientation="v", yanchor="bottom", y = -2)
             )
  ) }) 

  
  
  
  #PLOT 8 - FLEXIBLE TITLES
  dfcommontitles<- data.frame(read.csv("commontitles.csv"),stringsAsFactors = FALSE)
  dfcommontitles$Description <- factor(dfcommontitles$Description, levels  =unique(dfcommontitles$Description)[order(dfcommontitles$count, decreasing = TRUE)])
  output$plot8<- renderPlotly({print(
    plot_ly(
      data = dfcommontitles, 
      x = ~Description, 
      y = ~count, 
      type = 'bar', 
      hoverinfo='text',
      marker=list(color='#5DADE2'),
      height = 500,
      text = ~paste(
        'Title: ', Description,
        '<br> Agencies: ', count) ) %>%
      layout(title="Titles across the most agencies, excluding Community Boards", margin = list(b = 200), xaxis = list(tickangle = 90))
   ) }) 
  
  
}

#############################################

# Run the application 
shinyApp(ui = ui, server = server)

