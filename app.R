#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

electricity <- read.csv("/Users/cc/Desktop/ENVS401/CChen_ES401_ShinyApp/data/Electric.csv")
electricity <- electricity[-(1:2),]
# Rename the column
electricity <- electricity %>% 
  rename(Building_Name = X,
         Months= X.1,
         Consumption = X.2,
         Cost = X.3)
# Convert the E variables to numeric
electricity$Months <- as.numeric(electricity$Months)
electricity$Consumption <-as.numeric(electricity$Consumption)
electricity$Cost <-as.numeric(electricity$Cost)
# Convert Month
electricity <- electricity %>% 
  mutate(Months = factor(month.abb[Months], levels = month.abb)) %>%
  mutate(Type = "Electricity")

# Now we do the same thing for fuel
fuel <- read.csv("/Users/cc/Desktop/ENVS401/CChen_ES401_ShinyApp/data/Fuel.csv")
fuel <- fuel %>%
  rename(Building_Name = Building,
         Consumption = Consumption..Gallon.,
         Cost = Bill...)
fuel$Consumption <- as.numeric(fuel$Consumption)
fuel$Cost <- as.numeric(fuel$Cost)
fuel <- fuel %>% 
  mutate(Months = factor(month.abb[Months], levels = month.abb)) %>%
  mutate(Type = "Fuel")



# Now we have transportation left! Update the mega data set
transportation <- read.csv("/Users/cc/Desktop/ENVS401/CChen_ES401_ShinyApp/data/Transportation.csv")
transportation <- transportation %>%
  rename(Building_Name = Building,
         Consumption = Consumption..Gallon.,
         Cost = Bill...)
transportation$Consumption <- as.numeric(transportation$Consumption)
transportation$Cost <- as.numeric(transportation$Cost)
transportation <- transportation %>% 
  mutate(Months = factor(month.abb[Months], levels = month.abb)) %>%
  mutate(Type = "Transportation")


# Combine all three datasets together
df <- rbind(electricity, fuel, transportation)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Municipal Energy Consumption Tracking in Roxbury, VT"),
    radioButtons("choice1",
                 "Pick a sector among Electricity, Fuel, and Transportation that you would like to view",
                 c("Electricity", "Fuel", "Transportation")),
    selectInput("select1",
                "Choose a Building that you would like to view",
                c("Community Hall", "Roxbury Free Library", "Fire Station")),
    plotOutput(outputId = "graph"),
    dataTableOutput(outputId = "dataset")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  consumptionColor <- "#69b3a2"
  priceColor <- rgb(0.2, 0.6, 0.9, 1)
 # Plot the graph

  output$graph <- renderPlot({
    # Change it to mega data set
    df %>% filter(Building_Name == input$select1,
                           Type == input$choice1)  %>%
      ggplot(aes(x = Months, y = Consumption)) +
      geom_bar(stat='identity', fill = consumptionColor, color = "black", alpha = 0.3) +
      geom_line(aes(y= Cost / 0.5), size=2, color=priceColor) +
      scale_y_continuous("Consumption ", sec.axis = sec_axis(~.*0.5, name = "Cost ($)")) + 
      ggtitle(paste(input$choice1, "Consumption For", input$select1)) +
      theme(title=element_text(size=20),
            axis.title=element_text(size=16))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

