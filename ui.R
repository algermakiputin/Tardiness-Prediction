library(RMySQL)
library(DBI)
library(e1071)
library(naivebayes)
library(shiny)
library(shinycssloaders)
library(shinyjs)

ui <<- fluidPage(
  
  useShinyjs(),
  tags$head(
      tags$link(includeCSS("www/style.css")),
      includeScript("www/script.js")
    ),
  
  
  
  fluidRow(
    
    
    tags$div(class="navbar navbar-default",
             tags$div(class="container",
                      tags$div(class="navbar-header",
                               tags$a("Tardiness Prediction of Employees - Holy Child College of Davao",class="navbar-brand")
                      ),
                      tags$div(class="navbar-right",
                               actionButton("newemployee", "New Employee", class="btn btn-default btn-sm navbar-btn", style="margin-right:10px;")
                      )
             )
             
    ),
    tags$div(class="container", 
             
             fluidRow(style="padding:10px 0;",
                      column(
                        width = 12,
                        column(width = 3,class="topmenu",
                               style="border-right:0; height:78px;"
                               , 
                               
                               selectInput("campus", "Select Campus",
                                           choices = list("Mintal" = 1, "Jacinto" = 2), selected = 1
                               )
                               
                        ),
                        column(width = 9,class="topmenu text-center align-middle",style="border-left:0;height:78px;",
                               
                               actionButton("plotage", "Age"),
                               actionButton("plotyears", "Years"),
                               actionButton("plotdepartment", "Department"),
                               actionButton("plotstatus", "Status"),
                               actionButton("plottenure", "Tenure"),
                               actionButton("ploteducation", "Education") 
                               
                               
                               
                        )
                      ),
                      
                      
                      column(width = 3,
                             h4(id="label_emp","Employees"),
                             tags$ul(id="emp_list",
                                     withSpinner(uiOutput("list", height="auto"))
                             )
                             
                      ),
                      column(width = 9,
                             
                             fluidRow(
                               column(width = 12, style = "height: 500px",
                                      withSpinner(plotOutput("plot",width="100%"))
                                      
                                      
                                      ),
                               tags$div(style="padding-top:20px;display:none", id ="results", class="text-center",
                                       
                                       
                                       column(width = 3, style = "margin-left:14.5%", 
                                              tags$div(
                                                class = "result",
                                                "Late",h4(class="text-center",textOutput("class")))
                                              
                                       ),
                                       column(width = 3,
                                              tags$div(
                                                class = "result",
                                                "Yes",h4(class="text-center",textOutput("yes")))
                                              
                                       ),
                                       column(width = 3,
                                              tags$div(
                                                class = "result",
                                                "No",h4(class="text-center",textOutput("no")))
                                              
                                       )
                                       
                                       
                                       
                               )
                               
                             )
                             
                             
                             
                      )
                      
             )
             
    )
  )
  
  
    )