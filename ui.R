#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(readxl)


wineQualityReds <- read_csv("wineQualityReds.csv")
wineQualityReds$X1 <- NULL


ui <- dashboardPage(
                    dashboardHeader(color = "black",
                                    inverted = TRUE,
                                    dropdownMenuOutput("dropdown")
                                    ),
                    dashboardSidebar(
                                    # size = "thick", color = "green",
                                    sidebarMenu(id ="sidebarmenu", 
                                                menuItem(tabName = "Description", "Description",
                                                         menuSubItem(tabName = "Data", "Data",icon = icon("table")),
                                                         menuSubItem(tabName = "Summary", "Summary",icon = icon("receipt")),
                                                         menuSubItem(tabName = "Structure", "Structure",icon = icon("book"))
                                                        ),
                                                menuItem(tabName = "Univariant", "Univariant",
                                                         menuSubItem(tabName = "fixed acidity", "Fixed Acidity"),
                                                         menuSubItem(tabName = "volatile acidity", "Volatile Acidity"),
                                                         menuSubItem(tabName = "citric acid", "Citric Acid"),
                                                         menuSubItem(tabName = "residual sugar", "Residual Sugar"),
                                                         menuSubItem(tabName = "chlorides", "Chlorides"),
                                                         menuSubItem(tabName = "free sulfur dioxide", "Free Sulfur Dioxide"), 
                                                         menuSubItem(tabName = "total sulfur dioxide", "Total Sulfur Dioxide"),
                                                         menuSubItem(tabName = "density", "Density"),
                                                         menuSubItem(tabName = "pH", "pH"),
                                                         menuSubItem(tabName = "sulphates", "Sulphates")                                                        
                                                        ),
                                                menuItem(tabName = "Boxplot", "Boxplot"),
                                                menuItem(tabName = "Multivariant Distance Based", "Multivariant Distance Based", icon = icon("table"),
                                                         menuSubItem(tabName = "Mahalanobis Distance", "Mahalanobis Distance"),
                                                         menuSubItem(tabName = "Cook's Distance", "Cook's Distance")
                                                        ),
                                                menuItem(tabName = "Multivariant Density Based", "Multivariant Density Based", icon = icon("table"),
                                                         menuSubItem(tabName = "DBscan", "DBscan")
                                                ),
                                                menuItem(tabName = "Multivariant Outlier Detection", "Multivariant Outlier Detection", icon = icon("table"),
                                                         menuSubItem(tabName = "Local Outlier Factor", "Local Outlier Factor"),
                                                         menuSubItem(tabName = "Support Vector Machines", "Support Vector Machines")
                                                )
                                    )
                    ),
                    
                    
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "Data",
                          fluidRow(
                            DT::dataTableOutput("Data")
                          )
                        ),
                        tabItem(
                          tabName = "Summary",
                          fluidRow(
                            verbatimTextOutput("sum")
                          )
                        ),
                        tabItem(
                          tabName = "Structure",
                          fluidRow(
                            verbatimTextOutput("Structure")
                          )
                        ),                                                                                      
                        tabItem(
                          tabName = "fixed acidity",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("fixed_acidity"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Afixed_acidity"))
                            )
                          )                                                    
                        ),  
                        tabItem(
                          tabName = "volatile acidity",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("volatile_acidity"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Avolatile_acidity"))
                            )
                          )                                                    
                        ),       
                        tabItem(
                          tabName = "citric acid",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("citric_acid"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Acitric_acid"))
                            )
                          )                                                    
                        ),  
                        tabItem(
                          tabName = "residual sugar",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("residual_sugar"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Aresidual_sugar"))
                            )
                          )                                                    
                        ),                                              
                        tabItem(
                          tabName = "chlorides",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("chlorides"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Achlorides"))
                            )
                          )                                                    
                        ),  
                        tabItem(
                          tabName = "free sulfur dioxide",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("free_sulfur_dioxide"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Afree_sulfur_dioxide"))
                            )
                          )                                                    
                        ),       
                        tabItem(
                          tabName = "total sulfur dioxide",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("total_sulfur_dioxide"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Atotal_sulfur_dioxide"))
                            )
                          )                                                    
                        ),  
                        tabItem(
                          tabName = "density",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("density"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Adensity"))
                            )
                          )                                                    
                        ),
                        tabItem(
                          tabName = "pH",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("pH"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("ApH"))
                            )
                          )                                                    
                        ),                                             
                        tabItem(
                          tabName = "sulphates",
                          fluidRow(
                            box(width = 8,
                                title = "Before YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("sulphates"))
                            ),
                            box(width = 8,
                                title = "After YJ Transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Asulphates"))
                            )
                          )                                                    
                        ),
                        tabItem(
                          tabName = "Boxplot",
                          fluidRow(
                            box(width = 8,
                                title = "Before Transform check",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("boxplot")),
                                checkboxInput(inputId = "outliers1", label = "Show outliers", value = TRUE),
                                sliderInput(inputId = "range1", label = "IQR Multiplier", min = 0, max = 5, step=0.1,value = 1.5)
                            ),
                            box(width = 8,
                                title = "After Transform check",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Aboxplot")),
                                checkboxInput(inputId = "outliers2", label = "Show outliers",value = TRUE),
                                sliderInput(inputId = "range2", label = "IQR Multiplier", min = 0, max = 5, step=0.1,value = 1.5)
                            )
                          )                                                    
                        ), 
                        tabItem(
                          tabName = "Mahalanobis Distance",
                          fluidRow(
                            box(width = 8,
                                title = "BEFORE YJ transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("mah"))
                            ),
                            box(width = 8,
                                title = "After YJ transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Amah"))
                            )
                          ) 
                        ),
                        tabItem(
                          tabName = "Cook's Distance",
                          fluidRow(
                            box(width = 8,
                                title = "BEFORE transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("cook"))
                            ),
                            box(width = 8,
                                title = "After transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("Acook"))
                            )
                          ) 
                        ),
                        tabItem(
                          tabName = "DBscan",
                          fluidRow(
                            box(width = 8,
                                title = "BEFORE transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("DBscan")),
                                sliderInput(inputId = "scan", label = "Distance selector", min = 0, max = 50, step=2,value = 10)
                            ),
                            box(width = 8,
                                title = "After transform",
                                color = "black", ribbon = TRUE, title_side = "top right",
                                column(width = 8,plotOutput("ADBscan")),
                                sliderInput(inputId = "Ascan", label = "Distance selector", min = 0, max = 2, step=0.1,value = 0.5)
                            )
                          ) 
                        ),
                        tabItem(
                          tabName = "Local Outlier Factor",
                          fluidRow(
                             DT::dataTableOutput(outputId = 'lof')
                          ) 
                        ),
                        tabItem(
                          tabName = "Support Vector Machines",
                          fluidRow(
                                DT::dataTableOutput(outputId = 'SVM')
                            )
                          ))
                      ),
                      theme = "cerulean"
                  )
