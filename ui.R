library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)


# Define UI for application that plots random distributions 
shinyUI(
  navbarPage("LEGO dashboard",theme = shinytheme("cerulean"),
                   
                   tabPanel("Time analysis",
                            sidebarLayout(
                              position = "left",
                              sidebarPanel(h3("User Panel"),
                                           sliderInput("range", 
                                                       label = h5("Range of dates"),
                                                       min = 1950, max = 2020, value = c(1970, 2000))
                              ),
                              
                              mainPanel("",
                                        plotOutput(outputId = 'plot1'),
                                        plotOutput(outputId = 'plot2')
                              )
                            )
                   ),
                   
                   
                   
                   tabPanel("Themes",
                            sidebarLayout(
                              position = "left",
                              sidebarPanel(h3("User Panel"),
                                           sliderInput("range2", 
                                                       label = h5("Range of dates"),
                                                       min = 1950, max = 2020, value = c(1970, 2000)),
                                           
                                           h3('Themes'),
                                           radioButtons("radio2",p("(only for the first plot)"),
                                                        choices = list("Technic" = "Technic", "City" = "City",
                                                                       "Star Wars" = "Star Wars", "Duplo" = "Duplo",
                                                                       "All" = "All"),selected = "Technic")
                              ),
                              mainPanel("",
                                        plotOutput(outputId = 'plot3'),
                                        plotOutput(outputId = 'plot4')
                              )
                            )
                            
                   ),
             tabPanel("Statistical Analysis",
                      sidebarLayout(
                        position = "left",
                        sidebarPanel(h3("User Panel"),
                                     radioButtons("radio3",h3("Choose color"),
                                                  choices = list("blue" = "blue", "green" = "green", "yellow" = "yellow", "lightblue" = "lightblue"),selected = "lightblue")
                      ),

             mainPanel("",
                       plotOutput(outputId = 'plot5')
             )
             )
             ),
             
             navbarMenu("More",
                        tabPanel(tags$a(href="https://www.kaggle.com/rtatman/lego-database", "Dataset")),
                        tabPanel(tags$a(href="https://shiny.rstudio.com/", "Shiny")))
  )

)
