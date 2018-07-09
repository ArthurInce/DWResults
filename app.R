## canoe marathon results app

#-----------LIBRARY LOADING---------------

library(shiny)
library(shinythemes)
library(tidyverse) # group of packages for data wrangling and visualisation (ggplot2)
library(lubridate) # convert date to R date format
library(DT) # Data Tables package for interactive html tables

#-------------DATASET LOADING--------------------

main_data <- readRDS("CanoeingResults.rds") %>% 
  mutate(Outcome = case_when(is.na(Outcome) ~ "Finish", TRUE ~ Outcome)) %>% 
  mutate(Time = format(Time, format = "%H:%M:%S")) %>% 
  select(RaceRegion, Event, Date, Season, Race, Position, Name, Club, Class, Time, Points, Outcome, `P/D`) %>% 
  arrange(desc(Season), Event, Date, Race, Position) %>% 
  mutate(Name2 = paste(Name, "(", Club, ")"))

table_data <- main_data %>% 
  select(-Name2, -RaceRegion)

menus <- main_data %>% 
  select(Season, Event, Race) %>% 
  arrange(desc(Season), Event, Race) %>% 
  unique()

paddlers <- main_data %>% 
  select(Name2) %>% 
  arrange(Name2) %>% 
  unique()

regions <- main_data %>% 
  select(RaceRegion) %>% 
  arrange(RaceRegion) %>% 
  unique()

divs <- main_data %>% 
  select(Race) %>% 
  arrange(Race) %>% 
  unique()

top3s <- main_data %>% 
  select(Name2, Position) %>% 
  filter(Position %in%(c(1,2,3))) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()

medals <- main_data %>% 
  select(Name2, Position, Event) %>% 
  filter(Position %in%(c(1,2,3)), Event == "National Championships") %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()

completeCount <- main_data %>% 
  select(Name2, Position) %>% 
  filter(!is.na(Position)) %>% 
  select(-Position) %>% 
  group_by(Name2) %>% 
  add_tally() %>% 
  unique()

faveEvents <- main_data %>%
  select(Name2, Event) %>% 
  group_by(Name2, Event) %>% 
  add_tally() %>% 
  filter(n>1) %>% 
  arrange(Name2, -n) %>% 
  unique()

rankings <- main_data %>%
  select(Season, Name2, Position) %>% 
  group_by(Season, Name2) %>% 
  add_tally() %>% 
  unique() %>% 
  filter(n>2) %>% 
  summarise(aveFinish = mean(Position, na.rm = TRUE))

qualstatus <- main_data %>% 
  filter(Season == "2017/18", str_detect(Race, "Div") == TRUE, !Outcome %in% c("DNS", "RTD", "DSQ", "rtd", "dsq", "dns")) %>% 
  mutate(Qualname = paste(Name, "-", Club, "-", Class)) %>% 
  select(RaceRegion, Event, Season, Race, Qualname, Club) %>% 
  group_by(Qualname) %>% 
  mutate(RaceCt = n()) %>% 
  ungroup() %>% 
  arrange(Club, desc(RaceCt), Qualname)

clubdata <- readRDS("CanoeingClubPts.rds") %>% 
  select(`Race Name`, Club, `Hasler points`, Region) %>% 
  group_by(Club) %>% 
  mutate(Total = sum(`Hasler points`)) %>% 
  ungroup() %>% 
  arrange(Region)

promos <- main_data %>% 
  select(Season, Event, Name, Club, `P/D`, Date) %>% 
  filter(str_detect(`P/D`,"P")) %>% 
  filter(Date > today()-months(1)) %>% #change to 1 month from start of season
  arrange(`P/D`, Date)

#-------------UI SECTION ----
ui <- fluidPage(
  
  tags$head(includeHTML("GA.html")), #links to CofI client GA script
  
  theme = shinytheme("flatly"),
  
  list(tags$head(HTML('<link rel="icon", href="kayak.png", 
                      type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="Marathon Results Database"
      )
  ),
  
  fluidRow(column(10, tags$h1("Marathon Racing Committee Results Database")),
           column(2, tags$img(src="BritishCanoeing_LOGOCMYK.png", height = "80px", align = "left"))),
  
  tabsetPanel(
    
      tabPanel("Intro",
               
               fluidRow(column(12, tags$img(src="header.jpg", width = "100%"), tags$h6("Image copyright: Ollie Harding / Olypics"))),
               fluidRow(column(12, tags$h3("Congratulations to those winning a promotion in the last month:"))),
               fluidRow(column(12, DT::dataTableOutput("promotable")))
      ),
      
      tabPanel("Complete Database",
               fluidRow(column(12, tags$h3("Complete Database"), tags$h5("Filter all results since 2009/10 season in the table below"))),
               fluidRow(
                 column(4,
                        selectInput("season",
                                    "Pick one or more seasons:",
                                    c("All",
                                      unique(menus$Season)),
                                    selected = "2017/18",
                                    multiple = TRUE)
                 ),
                 column(4,
                        selectInput("event",
                                    "Pick one or more events:",
                                    c("All",
                                      unique(menus$Event)),
                                    selected = "All",
                                    multiple = TRUE)
                 ),
                 column(4,
                        selectInput("div",
                                    "Pick one or more Divisions/Classes:",
                                    c("All",
                                      unique(menus$Race)),
                                    selected = "All",
                                    multiple = TRUE)
                 )
               ),
               # Create a new row for the table.
               fluidRow(
                 column(12, DT::dataTableOutput("table"))
                 )),
      
      tabPanel("Paddler History", 
               fluidRow(
                 column(7, tags$h3(textOutput("paddlername"), tags$h5("Please note, paddlers may appear as duplicates in this system if they have raced for multiple clubs or if their names were entered incorrectly at races"))),
                 column(5,selectInput("paddler", "Choose Paddler: (hit backspace to clear and type in a name)", c(unique(paddlers$Name2)), selected = "JAMES BELL ( LON )", multiple = FALSE))
                        ),
               fluidRow(column(3, tags$h3("Races Entered"), tags$h4(textOutput("no_races")), tags$h4(textOutput("comprate"))),
                         column(9, plotOutput("races"))
                        ),
                
               fluidRow(column(3, tags$h3("Positions"), tags$h4(textOutput("top3s")), tags$h4(textOutput("medals"))),
                        column(9, plotOutput("positions"))
                        ),
                
               fluidRow(column(3, tags$h3("Home & Away"), tags$h4("Top races by no. of entries"), tableOutput("faveraces")),
                        column(9, plotOutput("travels"))
                        )
               ),
      
      tabPanel("Race Attendance", 
               fluidRow(
                 column(6, tags$h3("Race Attendance by Region")),
                 column(3,selectInput("attdiv", "Choose one or more Divisions/Classes:", c(unique(divs$Race)), selected = "Div5_5", multiple = TRUE)),
                 column(3,selectInput("region", "Choose Region:", c(unique(regions$RaceRegion)), selected = "MID", multiple = FALSE))
               ),
               fluidRow(column(12, plotOutput("attendance"))
               )
      ),
      
      tabPanel("Hasler Final Qualification", 
               fluidRow(
                 column(7, tags$h3("Qualifying Status by Paddler & Craft, 2017/18 Season"), tags$h5("Please note the list does not include Assessment Races which count towards Final qualification.")),
                 column(5,selectInput("qualclub", "Choose Club: (hit backspace to clear and type in a club abbreviation)", c(unique(qualstatus$Club)), selected = "BAN", multiple = FALSE))
               ),
               fluidRow(column(12, tableOutput("qualification"))
               )
      ),
      
      tabPanel("Club Regional Standings", 
               fluidRow(
                 column(7, tags$h3("Regional Clubs Leaderboard, 2017/18 Season"), tags$h5("Please refer to your Regional Marathon Advisor for the number of clubs qualifying for the Final from your region")),
                 column(5,selectInput("qualreg", "Choose Region:", c(unique(clubdata$Region)), selected = "SO", multiple = FALSE))
               ),
               fluidRow(column(12, tableOutput("clubpts"))
               )
      )
  )
  
  )
               
#----------------SERVER FUNCTION ---------------------

server <- function(input, output, session) {
  
  # Big Database table with reactive filtering
  output$table <- DT::renderDataTable({
    DT::datatable({
      data <- table_data
        if (input$season != "All") {
          data <- data[data$Season %in% input$season,]
        }
        if (input$event != "All") {
          data <- data[data$Event %in% input$event,]
        }
        if (input$div != "All") {
          data <- data[data$Race %in% input$div,]
        }
        data #i.e. render this dataset as a table
        },
      options = list(pageLength = 15), 
      rownames = FALSE)
    }
  )
  
  #filtered inputs for main table
  menus_filtered1 <- reactive({
    if ("All" %in% input$season) {
      menus
    } else {
    menus %>% filter(Season %in% input$season)}
  })
  
  observe({
    updateSelectInput(session, "event", choices = c("All", menus_filtered1()$Event), selected = "All")
  })
  
  menus_filtered2 <- reactive({
    if ("All" %in% input$event) {
      menus_filtered1()
    } else {
    menus_filtered1() %>% filter(Event %in% c(input$event))}
  })
  
  observe({
    updateSelectInput(session, "div", choices = c("All",menus_filtered2()$Race), selected = "All")
  })
  
  #define outputs for individual paddler
  output$paddlername <- renderText({paste("Paddler History:",input$paddler)})
  
  output$no_races <- renderText({
    paste(nrow(main_data[main_data$Name2 == input$paddler,]),
          "races entered,")
          })
  
  output$comprate <- renderText({
    paste(round(completeCount$n[completeCount$Name2 == input$paddler]/nrow(main_data[main_data$Name2 == input$paddler,])*100, digits = 0),
          "% completion rate")
    })
  
  output$top3s <- renderText({
    paste("Number of Top 3 finishes:",
          top3s$n[top3s$Name2 == input$paddler]
    )
    })
  
  output$medals <- renderText({
    paste("Number of National Championship medals:",
          medals$n[medals$Name2 == input$paddler]
    )
  })
  
  output$faveraces <- renderTable({
    faveraces1 <- faveEvents %>% 
      filter(Name2 == input$paddler) %>% 
      rename(Entries = n) %>% 
      head(5)
      
  }, rownames = FALSE)
  
  output$races <- renderPlot({
    
    racechartdata <- main_data %>% 
      select(Season, Name2) %>% 
      filter(Name2 == input$paddler)
    
    ggplot(racechartdata, aes(Season))+
      geom_histogram(aes(fill = Season), stat = "count")+
      ylab("Number of Races Entered")
    
  })
  
  output$positions <- renderPlot({
    
    poschartdata <- main_data %>% 
      select(Season, Position, Race, Name2) %>% 
      filter(Name2 == input$paddler)
    
    ggplot(poschartdata)+
      geom_point(aes(Season, Position, color = Race), position=position_jitter(width=0.1, height=0.1), size = 5)+
      scale_y_continuous(trans = "reverse", breaks = unique(main_data$Position))
    
  })
  
  output$travels <- renderPlot({
    
    regchartdata <- main_data %>% 
      select(Season, RaceRegion, Name2) %>% 
      filter(Name2 == input$paddler)
    
    ggplot(regchartdata, aes(Season))+
      geom_histogram(aes(fill = RaceRegion), stat = "count")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
  })
  
  output$attendance <- renderPlot({
    
    attchartdata <- main_data %>% 
      select(RaceRegion, Event, Season, Race) %>% 
      filter(RaceRegion == input$region, Race %in% input$attdiv)
    
    ggplot(attchartdata, aes(Event))+
      geom_bar(aes(fill = Race), stat = "count")+
      coord_flip()+
      facet_wrap(~Season)
    
  })
  
  output$qualification <- renderTable({
    qualdata <- qualstatus %>% 
      filter(Club == input$qualclub) %>% 
      select(Qualname, RaceCt, Event, Race) %>% 
      spread(Event, Race) %>% 
      arrange(desc(RaceCt), Qualname)
  }, rownames = FALSE)
  
  output$clubpts <- renderTable({
    clubqualdata <- clubdata %>% 
      filter(Region == input$qualreg) %>% 
      select(Club, `Race Name`, `Hasler points`, Total) %>%
      spread(`Race Name`, `Hasler points`) %>% 
      arrange(desc(Total), Club)
  }, rownames = FALSE)
  
  # Big Database table with reactive filtering
  output$promotable <- DT::renderDataTable({
    DT::datatable({
      promos #i.e. render this dataset as a table
    },
    options = list(pageLength = 15), 
    rownames = FALSE)
  }
  )
}

shinyApp(ui, server)