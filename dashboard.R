library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)
library(plyr)
library(stringr)
library(tidyverse)
library(grid)
library(devtools)
library(tidyr)


#read in data
crosses <- read.csv("crosses.csv")
events <- read.csv("events_np.csv")
matches <- read.csv("matches.csv")

events$shot.outcome.name[is.na(events$shot.outcome.name)] <- "Nothing"
events$shot.body_part.name[is.na(events$shot.body_part.name)] <- "None"
events$shot.type.name[is.na(events$shot.type.name)] <- "None"
events$goalkeeper.position.name[is.na(events$goalkeeper.position.name)] <- "None"

#use median for numeric values 
events$DistToGoal[is.na(events$DistToGoal)] <- median(events$DistToGoal, na.rm = T)
events$DistToKeeper[is.na(events$DistToKeeper)] <- median(events$DistToKeeper, na.rm = T)
events$AngleToGoal[is.na(events$AngleToGoal)] <- median(events$AngleToGoal, na.rm = T)
events$AngleToKeeper[is.na(events$AngleToKeeper)] <- median(events$AngleToKeeper, na.rm = T)

events$shotoutcome <- ifelse(events$shot.outcome.name == "Goal",1,0)

logit_model <- glm(events$shotoutcome ~ events$shot.body_part.name + events$shot.type.name + events$goalkeeper.position.name + events$DistToGoal + events$DistToKeeper + events$AngleToGoal + events$AngleToKeeper, family = binomial)
summary(logit_model)
events$shotprediction1 <- predict(logit_model, type = "response")


logit_model2 <- glm(events$shotoutcome ~ events$shot.body_part.name + events$shot.type.name + events$goalkeeper.position.name, family = binomial)
summary(logit_model2)
events$shotprediction2 <- predict(logit_model2, type = "response") 

exp(coef(logit_model))

filter_options <- c("Team", "Player")
team_name <- sort(unique(events$team.name))
player_name <- sort(unique(events$player.name))

ui <- fluidPage(
  br(),
  br(),
  tabPanel("Main",
           column(2, 
                  fluidRow(
                    box(title = "Control Panel", width = 12, height = 500,
                        radioButtons("filter", label="Select filter option: ", choices=filter_options, selected="Team"),
                        selectInput("TeamName", "Select team name", team_name),
                        selectInput("PlayerName", "Select player name", player_name)
                    )
                  )
           ),
           column(10,
                  tabsetPanel(
                    tabPanel("Shot Prediction",  DT::dataTableOutput("tab1"), height="500px"),
                    tabPanel("Pass Length",  plotOutput("bar1"), height="500px")
                  ),
      )
))

server <- function(input, output) {
  output$tab1 <- DT::renderDataTable({
    if (input$filter == "Team"){
      selected_country <- subset(events, team.name == input$TeamName)
      selected_country[,c("period", "timestamp", "type.name", "play_pattern.name", "team.name","player.name", "position.name", "shot.type.name", "shotprediction1", "shotprediction2")]
      
    }
    else if (input$filter == "Player"){
      selected_player <- subset(events, player.name == input$PlayerName)
      selected_player[,c("period", "timestamp", "type.name", "play_pattern.name", "team.name","player.name", "position.name", "shot.type.name", "shotprediction1", "shotprediction2")]
      
    }
  })
  output$bar1 <- renderPlot({
    if (input$filter == "Team"){
      selected_country <- subset(events, team.name == input$TeamName)
      ggplot(selected_country, aes(x=duration, y=pass.length, color=player.name)) + geom_point()
      
    }
    else if (input$filter == "Player"){
      selected_player <- subset(events, player.name == input$PlayerName)
      ggplot(selected_player, aes(x=duration, y=pass.length, size=pass.length)) + geom_point()
             
    }
  })
  
  
}

shinyApp(ui, server)