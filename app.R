#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Query Structure Reference
#
# query <- '**  query  **' %>%
#   call_neo4j(query, con,
#              type = c("row", "graph"),
#              output = c("r","json"),
#              include_stats = FALSE,
#              include_meta = FALSE)
#
## Then, one of these three depending on what *query* returns:
#
# unnest_nodes(nodes_tbl, what = c("all", "label", "properties")) # for node table
# unnest_relationships(relationships_tbl) # for relationship table
# unnest_graph(res) # for api graph result

library(shiny)
library(neo4r)
library(ggplot2)

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# ========Queries=========
return.medals.age = "with (n.medal) as `Tier`, size(collect(n.ID)) as `Medals`
return Tier, Medals"

medals.1 = "Match(n:olympics) where 10<=toInteger(n.age)<=39"
medals.2 = "Match(n:olympics) where 40<=toInteger(n.age)<=68"
medals.3 = "Match(n:olympics) where 69<=toInteger(n.age)<=97"
yearclause = "<=toInteger(n.year)<="
seasonclause = "n.season ="
# ========================

# Neo4R Connection Object
con <- neo4j_api$new(url = "100.25.118.163:33648", 
                     user = "neo4j",
                     password = "stare-pea-explosion")

# Load data from CSV only if unimported
# Capitalize the attributes you need!
if (dim(con$get_labels())[1] == 0) {
  # Stop app if data not found
  if (!file.exists("./dt/athlete_events.csv")) {stopApp("Data not found by R: Could not find ./dt/athlete_events.csv")}
  else{
  load_csv(on_load="create (a1: olympics {ID: olympics.ID, name: olympics.Name, sex: olympics.sex, age: olympics.Age, heights: olympics.heights, weights: olympics.weights, team: olympics.team, NOC: olympics.NOC, games: olympics.games, year: olympics.Year, season: olympics.Season, city: olympics.city, sports: olympics.sport, event: olympics.event, medal: olympics.Medal });",
           con=con,
           url="https://docs.google.com/spreadsheets/d/1-wGtrPbwIMGfwdlyDrcSXeEH0p6n2dEcObCgfl_FiDc/export?format=csv&id=1-wGtrPbwIMGfwdlyDrcSXeEH0p6n2dEcObCgfl_FiDc&gid=1049694386",
           as="olympics",
           output = "r"
  )
    }
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Olympic History Statistics"),
  
  h2("Outline:"),
  navlistPanel(widths = c(12,12),
    "Information",
    tabPanel("Home",
       h1("Data Summarizations from 1896-2016", align = "center"),
       h3("DS220-002 Lee, Group 10", align = "center"),
       h4("Dataset provided by Kaggle user heesoo37", align = "center"),
       h4(as.character(Sys.Date()), align = "center"),
       HTML('<p style="text-align:center;"><img src="olympic_rings.png" width="640" height="720" align="middle"/></p>')
    ),
    tabPanel("Background",
             "Hello! Thank you for checking out our Shiny Web App.",
             br(),
             "This was created as a school project by "
             ),
    "Summarization Tools",
    tabPanel("Age Range Performance",
             titlePanel("Age Range Performance by Year/Season"),
             "Breaks down how each age range performed in a given timespan for the summer or winter Olympics.",
             br(),
             dateRangeInput('dateRange',
                            label = "Selected Year: ",
                            format = "yyyy",
                            language="en",
                            start = Sys.Date() - 365 * 10,
                            end = Sys.Date(),
                            startview = "year"
                            ),
             fluidRow(
               column(width = 12,
                      selectInput("checkGroup_Age",
                                         width = '100%',
                                         label = "Selected Age Range(s)", 
                                         choices = list("10-39" = 1, "40-68" = 2, "69-97" = 3)
                                         ),

                      selectInput("select_Season",
                                  label = "Selected Olympic Season",
                                  choices = list("Summer" = "Summer", "Winter" = "Winter"), 
                                  selected = 1
                      ),
                      
                      textOutput("SliderText1"),
                      
                      textOutput("SliderText2"),
                      
                      textOutput("SliderText3")
                      
                      # plotOutput("hist_age")
               ),

               column(width = 12,
                      plotOutput("hist_age")
                      )
               )
             ),
    tabPanel("Summarization 1"),
    tabPanel("Summarization 1")
    )
)


# Define required server logic
server <- function(input, output) {

  Dates <- reactiveValues()
  observe({
    Dates$SelectedDates <- c(
      as.character(
        format(input$dateRange[1],
               format = "%Y")
        ),
      as.character(
        format(input$dateRange[2],
               format = "%Y")
        )
      )
    })

  Ranges = reactiveValues()
  observe({
    Ranges$SelectedRanges = c(as.character(input$checkGroup_Age))
  })
  
  Seasons = reactiveValues()
  observe({
    Seasons$SelectedSeasons = c(as.character(input$select_Season))
  })

  
  # Data = reactiveValues()
  # observe({
  #   if (1 %in% Ranges$SelectedRanges) {
  #     Data$data.age.1 = call_neo4j(con=con,
  #                             query=paste(medals.1, " and ", Dates$SelectedDates[1], yearclause, Dates$SelectedDates[2], "and", seasonclause, Seasons$SelectedSeasons)
  #     )
  #   }  else {Data$data.age.2 = data.frame()}
  # 
  #   if (2 %in% Ranges$SelectedRanges) {
  #     Data$data.age.2 = call_neo4j(con=con,
  #                             query=paste(medals.2, " and ", Dates$SelectedDates[1], yearclause, Dates$SelectedDates[2], "and", seasonclause, Seasons$SelectedSeasons)
  #     )
  #   }  else {Data$data.age.2 = data.frame()}
  # 
  #   if (3 %in% Ranges$SelectedRanges) {
  #     Data$data.age.2 = call_neo4j(con=con,
  #                             query=paste(medals.3, " and ", Dates$SelectedDates[1], yearclause, Dates$SelectedDates[2], "and", seasonclause, Seasons$SelectedSeasons)
  #     )
  #   }  else {Data$data.age.2 = data.frame()}
  # 
  #   isolate({sapply(Data, as.data.frame)})
  #   return(Data)
  # })
  

  output$SliderText1 <- renderText({paste("Years Selected: ", paste(Dates$SelectedDates, collapse = " - "))})
  output$SliderText2 <- renderText({paste("Age Ranges Selected: ", paste(Ranges$SelectedRanges, collapse = ", "))})
  output$SliderText3 <- renderText({paste("Seasons Selected: ", Seasons$SelectedSeasons)})
  # output$hist_age <- renderPlot({
  #   newdata = Data()
  #   ggplot() + geom_histogram(data=newdata$data.age.1, aes(x = Medals, y = Tier))
  # })
  }

# Run the application 
shinyApp(ui = ui, server = server)

