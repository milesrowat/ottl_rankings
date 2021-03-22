#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(DT)



load("data/rankings.rda")

event_groupings <- c("Sprints", "Middle Distance", "Long Distance","Walks","Road Running", "Hurdles","Relays","Jumps","Throws","Combined Events")
sprints <- c("60m","100m","150m","200m","300m","400m","500m")
mid_distance <- c("600m","800m","1000m","1200m","1500m","Mile","2000m","3000m")
long_distance <- c("5000m","10000m","Marathon","1500m Steeplechase", "2000m Steeplechase","3000m Steeplechase")
hurdles <- c("60m Hurdles", "80m Hurdles", "100m Hurdles", "110m Hurdles", "200m Hurdles", "300m Hurdles", "400m Hurdles")
relays <- c("4x100m","4x200m","4x400m","4x800m","Sprint Medley", "Distance Medley")
walks <- c("5000m RW","10km RW","20km RW")
jumps <- c("Long Jump","Triple Jump","High Jump","Pole Vault")
throws <- c("Shot Put", "Discus","Javelin","Hammer Throw")
ce <- c("Pentathlon","Heptathlon","Decathlon")
road <- c("10km","Half-Marathon","Marathon")

event_key <- list("Sprints"=sprints,"Middle Distance" = mid_distance, "Long Distance" = long_distance,"Hurdles" = hurdles,"Relays" = relays, "Walks" = walks, "Road Running" = road, "Jumps" = jumps, "Throws" = throws, "Combined Events" = ce)

createLink <- function(x,y) {
    sprintf('<a href="/sum_%s.html", target="_blank">%s</a>',x,y)
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    #Create page title
    dashboardHeader(title = "Ottawa Lions Rankings"),
    
    dashboardSidebar(
        selectInput("Sex",
                    "Please select a sex:",
                    choices = c("Men","Women")),
        
        uiOutput("group_selector"), #add select input boxes
        uiOutput("event_selector"), #from objects created in server
        
        radioButtons("Age_Class",
                     "Age Category",
                     choices = list("Open" = "Open_marker", "U23" = "U23_marker", "U20" = "U20_marker", "U18" = "U18_marker"))
    ),
    dashboardBody(
        box(title = "Age Records",
            tableOutput("records"), width = 400),
        box(title = "All-time rankings",
            dataTableOutput("table"), width = 600)
        
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$group_selector = renderUI({ #creates event group select box object called in ui
        selectInput(inputId = "event_group", #name of input
                    label = "Event Group:", #label displayed in ui
                    choices = event_groupings,  # displays unique values from the event_groupings vector column in the previously created table
                    selected = "Sprints") #default choice (not required)
    })
    
    output$event_selector = renderUI({#creates event select box object called in ui
        
        data_available = event_key[input$event_group]
        #creates a reactive list of available counties based on the State selection made
        
        selectInput(inputId = "event", #name of input
                    label = "Event:", #label displayed in ui
                    choices = data_available, #calls list of available events
                    selected = (data_available[1]))
    })
    
   
    output$table <- renderDataTable({
        
        rankings_process <- rankings %>% 
            filter(Sex == input$Sex, Event == input$event, get(input$Age_Class) == 1)# %>%
            
        ifelse(rankings_process$Event %in% c("Long Jump","Triple Jump","High Jump","Pole Vault","Shot Put", "Discus","Javelin","Hammer Throw","Pentathlon","Heptathlon","Decathlon"),
               rankings_process <- rankings_process %>% arrange(desc(Time.Distance)) %>% distinct(Name, .keep_all = TRUE),
               rankings_process <- rankings_process %>% arrange(Time.Distance) %>% distinct(Name, .keep_all = TRUE))
        
        rankings_publish <- rankings_process %>% 
            mutate(ID = tolower(paste(Last.Name,First.Name,YOB,sep = "_"))) %>%
            select(Performance = Time.Distance, Wind, Name, YOB, Location, Date = Long_Date, ID) 
        rankings_publish$Name <- createLink(rankings_publish$ID,rankings_publish$Name) #add second variable here to write the name above
        
        return(rankings_publish %>% select(Performance, Wind, Name, YOB, Location, Date))
    }, escape = FALSE)
    
    output$records <- renderTable({
        
        records_process <- rankings %>% 
            filter(Sex == input$Sex, Event == input$event)
        
        age_class <- c('Open','U23','U20','U18')
        record_class <- c("Open_marker","U23_marker","U20_marker","U18_marker")
        record_key <- data.frame(age_class,record_class)
        
        
        record <- function(x){
            rec <- records_process %>%
                filter(get(x) == 1) 
            
            ifelse(rec$Event %in% c("Long Jump","Triple Jump","High Jump","Pole Vault","Shot Put", "Discus","Javelin","Hammer Throw","Pentathlon","Heptathlon","Decathlon"),
                   rec <- rec %>% arrange(desc(Time.Distance)),
                   rec <- rec %>% arrange(Time.Distance)
            )
            
            rec[1, ]
        }
        
        
        as.data.frame(t(sapply(record_key$record_class,record))) %>% mutate(Record = paste("Lions",record_key$age_class,"Record")) %>% select(Record, Performance = Time.Distance, Wind, Name, YOB, Location, Date = Long_Date)
        
        
            
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
