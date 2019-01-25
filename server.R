library(RMySQL)
library(DBI)
library(e1071)
library(naivebayes)
library(shiny)
library(shinycssloaders)
library(shinyjs)

classifier = "";
nb = "";

server <<- function(input, output) {
  observeEvent(input$newemployee, {
    showModal(
      modalDialog(id ="modal",title = "New Employee",
                  tags$div(class="form-group",
                           tags$input(name = "name", type="text", placeholder="Employee Name", class="form-control", required = "required")
                  ),
                  tags$div(class="form-group",
                           tags$input(name = "years", type="text", placeholder="Years of Experience", class="form-control", required = "required")
                  ),
                  tags$div(class="form-group",
                           tags$select(name = "marital-status", class="form-control", required = "required",
                                       tags$option(value="single", "Single"),
                                       tags$option(value="married", "Married"),
                                       tags$option(value="divorce", "Divorce"),
                                       tags$option(value="widowed", "Widowed")
                           )
                  ),
                  tags$div(class="form-group",
                           tags$input(name = "age", type="number", placeholder="Age", class="form-control", required = "required")
                  ),
                  tags$div(class="form-group",
                           tags$select(name = "education", class="form-control", required = "required",
                                       tags$option(value="Associate Degree", "Associate Degree"),
                                       tags$option(value="Bachelor Degree", "Bachelor Degree"),
                                       tags$option(value="Master Degree", "Master Degree"),
                                       tags$option(value="Doctoral Degree", "Doctoral Degree")
                           )
                  ),
                  
                  tags$div(class="form-group",
                           tags$select(name = "tenure", class="form-control", required = "required",
                                       tags$option(value="", "Permanent Employee"),
                                       tags$option(value="0", "No"),
                                       tags$option(value="1", "Yes") 
                           )
                  ),
                  tags$div(class="form-group",
                           tags$select(name = "department", class="form-control", required = "required",
                                       tags$option(value="", "Select Department"),
                                       tags$option(value="college", "College"),
                                       tags$option(value="highschool", "High School") 
                           )
                  ),
                  footer = tags$button(id="addnewemployee", "Add" , class="btn btn-primary"),
                  easyClose = TRUE
      )
    )  
  })
  
  observeEvent(input$campus, {
    output$plot<- renderPlot({
      classifier <<- getDataset(input$campus);
    
      if (classifier != "") {
        nb <<- naive_bayes(late ~ ., data = classifier)
        (plot(nb,"years"))
        shinyjs::enable("plotage")
        shinyjs::enable("plotyears")
        shinyjs::enable("plotdepartment")
        shinyjs::enable("plotstatus")
        shinyjs::enable("plottenure")
        shinyjs::enable("ploteducation")
      }else {
        plot(0)
        shinyjs::disable("plotage")
        shinyjs::disable("plotyears")
        shinyjs::disable("plotdepartment")
        shinyjs::disable("plotstatus")
        shinyjs::disable("plottenure")
        shinyjs::disable("ploteducation")
      }
      
      shinyjs::hide("results");
     
    }, height =  560)
  })
  
  observeEvent(input$plotage, {
    output$plot <- renderPlot({
      
      plot(nb,"age")
    }, height = 560)
    
  })
  
  observeEvent(input$plotyears, {
    output$plot <- renderPlot({
      
      plot(nb,"years")
    }, height = 560)
    
  })
  observeEvent(input$ploteducation, {
    output$plot <- renderPlot({
      
      plot(nb,"education")
    }, height = 560)
    
  })
  observeEvent(input$plotstatus, {
    output$plot <- renderPlot({
      
      plot(nb,"marital_status")
    }, height = 560)
    
  })
  observeEvent(input$plottenure, {
    output$plot <- renderPlot({
      
      plot(nb,"tenure")
    }, height = 560)
    
  })
  observeEvent(input$plotdepartment, {
    output$plot <- renderPlot({
      
      plot(nb,"department")
    }, height = 560)
    
  })
  
  
  sample.int(3 , 100 , replace = T)
  onevent("change", "campus", 
          output$lb <- renderText(input$campus),
          output$list <- renderUI({
            
            q<- paste("SELECT CONCAT(first_name, ' ', last_name) as name,marital_status,tenure,education,department_id,birthday, date_joining FROM employees WHERE campus_id=", input$campus)
            employees <- sqlQuery(q)
            
            if (nrow(employees)) {
              colnames(employees) <- c("Name", "Marital Status", "tenure", "education", "department_id","birthday", "date_joining")
              
              LL <- vector("list",10)        
              for(i in 1:nrow(employees)){
                departmentName <- sqlQuery(paste("SELECT name FROM departments WHERE id =", employees[i,c("department_id")]))
                colnames(departmentName) <- c("name")
                age = floor(as.numeric( as.Date(Sys.Date()) - as.Date(employees[i,c("birthday")], "%Y-%m-%d" ) ) / 365.25)
                years = floor(as.numeric( as.Date(Sys.Date()) - as.Date(employees[i,c("date_joining")], "%Y-%m-%d" ) ) / 365.25)
                LL[[i]] <- list(tags$li(employees[i,c("Name")], class ="li emp-list",
                                        "data-status"= as.character(employees[i,c("Marital Status")]), 
                                        "data-years" = as.numeric(years),
                                        "data-education" = as.character(employees[i, c("education")]),
                                        "data-tenure" = as.character(employees[i, c("tenure")]),
                                        "data-department" = as.character(departmentName[1,c("name")]),
                                        "data-age" = as.numeric(age)
                                        
                ))
                
              } 
              return(LL)
            }else {
              return(tags$p("No records found", style="padding:5px;"))
            } 
            
          })
  )
  
  p<- function(x, session, inputname) {
    employeeData = data.frame(
      "years" = as.integer(x['years']),
      "marital_status"= as.character(x['status']),
      'age' = as.integer(x['age']),
      'education' = as.character(x['education']),
      'tenure' = as.character(x['tenure']),
      'department' = as.character(x['department'])
    );
    
    class<- predict(nb, employeeData, type = "class")
    
    showModal(
      modalDialog(id ="prediction-result",title = "Prediction Result:",
                  tags$div(style="padding:20px", id ="results", class="text-center",
                           h3(x['name']),
                           fluidRow(
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
                           
                           
                           
                           
                           
                  ),
                  easyClose = TRUE
      )
    )  
    
    probability <- predict(nb, employeeData, type = "prob")
    yes = format(round(as.double(probability[1,"yes"]) * 100), digits=2)
    no = format(round(as.double(probability[1,"no"]) * 100), digits=2)
 
    shinyjs::hide("results");

    output$yes <- renderText(paste(yes,"%"));
    output$no <- renderText(paste(no,"%"));
    classes = list("Yes", "No")

    output$class <- renderText(classes[[as.integer(class)]]);
    shinyjs::show("results");
 
  }
  removeInputHandler("s")
  registerInputHandler("s", p)
 
  
  }