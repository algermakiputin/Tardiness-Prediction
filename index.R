library(shiny)
library(DT)
library(shinyjs)
library(e1071)
library(klaR)
library(naivebayes)

source("~/R/Shiny/DBConnect.R")
classifier = null;
nb = null;
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$style("
              .result {padding:15px;text-align:center;border:solid 1px #ddd;vertical-align:middle }
              ul {list-style:none;padding:0;border:solid 1px #eee; border-bottom:0}
              .emp-list{padding:10px;border-bottom:solid 1px #eee}
              .topmenu {background-color:#f4f4f5;border:solid 1px #ddd;min-height:76px;vertical-align:middle}
              .align-middle {padding:20px 0}
              #emp_list li:hover {background-color: #eee;cursor:pointer;box-shadow:0 0 0 3px #eee;color:#777}
              .emp-list.active {background-color:#eee;box-shadow:0 0 0 3px #eee;color:#777}
               ")
  ),
 
  
  fluidRow(
    
    column(width = 10, offset = 1, 
            
           fluidRow(style="padding:30px 0;",
             column(
               width = 12,
               column(width = 3,class="topmenu",
                      style="border-right:0"
                      , 
                      
                      selectInput("campus", "Select Campus",
                                  choices = list("Mintal" = 1, "Jacinto" = 2), selected = 1
                      )
                      
                      
               ),
               column(width = 9,class="topmenu text-center align-middle",style="border-left:0",
                    
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
                      uiOutput("list")
                    )
                     
                      
               ),
               column(width = 9,
                  
                  plotOutput("plot",width="100%")
                    
                       
                      
               ),column(width = 9, offset =2,style="padding-top:20px;",
                     
                    column(width = 3,offset=3,
                           tags$div(
                             class = "result",
                             "Tardy",h4(class="text-center",textOutput("class")))
                            
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

server <- function(input, output) {
 
  output$plot<- renderPlot({
      classifier <<- getDataset(1);
      nb <<- naive_bayes(late ~ .,  classifier)
      plot(nb)
  })
  observeEvent(input$campus, {
    output$plot<- renderPlot({
      classifier <<- getDataset(input$campus);
      nb <<- naive_bayes(late ~ ., data = classifier, userkernel = true)
      if (!as.integer(nrow(classifier)) >= 10) {
        classifier<<- 0
        return(plot(0))
      }
        
      plot(nb,"years",col="yellow")
    })
  })
  
  observeEvent(input$plotage, {
    output$plot <- renderPlot({
      plot(nb,"age")
    })
    
  })
  
  observeEvent(input$plotyears, {
    output$plot <- renderPlot({
      plot(nb,"years")
    })
    
  })
  observeEvent(input$ploteducation, {
    output$plot <- renderPlot({
      plot(nb,"education")
    })
    
  })
  observeEvent(input$plotstatus, {
    output$plot <- renderPlot({
      plot(nb,"marital_status")
    })
    
  })
  observeEvent(input$plottenure, {
    output$plot <- renderPlot({
      plot(nb,"tenure")
    })
    
  })
  observeEvent(input$plotdepartment, {
    output$plot <- renderPlot({
      plot(nb,"department")
    })
    
  })

  
  sample.int(3 , 100 , replace = T)
  onevent("change", "campus", 
          output$lb <- renderText(input$campus),
          output$list <- renderUI({
        
            q<- paste("SELECT CONCAT(first_name, ' ', last_name) as name,marital_status,tenure,education,department_id,birthday, date_joining FROM employees WHERE campus_id=", input$campus)
            employees <- sqlQuery(q)
            colnames(employees) <- c("Name", "Marital Status", "tenure", "education", "department_id","birthday", "date_joining")
            
            LL <- vector("list",10)        
            for(i in 1:nrow(employees)){
              departmentName <- sqlQuery(paste("SELECT name FROM departments WHERE id =", employees[i,c("department_id")]))
              colnames(departmentName) <- c("name")
              age = floor(as.numeric( as.Date(Sys.Date()) - as.Date(employees[i,c("birthday")]) ) / 365.25)
              years = floor(as.numeric( as.Date(Sys.Date()) - as.Date(employees[i,c("date_joining")]) ) / 365.25)
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
    
    probability <- predict(nb, employeeData, type = "prob")
    yes = format(round(as.double(probability[1,"yes"]) * 100), digits=2)
    no = format(round(as.double(probability[1,"no"]) * 100), digits=2)
    
    output$yes <- renderText(paste(yes,"%"));
    output$no <- renderText(paste(no,"%"));
    classes = list("Yes", "No")
  
   
    output$class <- renderText(classes[[as.integer(class)]]);
  }
  removeInputHandler("s")
  registerInputHandler("s", p)
  
  
  shinyjs::runjs(
    '
      $("#emp_list").on("click",".li",function(){
        
          $(".emp-list").removeClass("active");
          $(this).addClass("active");
 
          status = $(this).data("status");
          years = $(this).data("years");
          education = $(this).data("education");
          tenure = $(this).data("tenure");
          department = $(this).data("department");
          age = $(this).data("age");
    
          Shiny.setInputValue("coord:s", {
                                  status : status,
                                  years : years,
                                  education : education,
                                  tenure : tenure,
                                  department : department,
                                  age : age
                              });
    
      });
      
     
    '
  )
  
}

shinyApp(ui = ui, server = server)


  