library(shiny)
library(DT)
library(shinyjs)
library(e1071)
library(klaR)
library(naivebayes)
 
source(file = "DBConnect.r")

ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$style("
              ul {list-style:none;padding:0;border:solid 1px #eee; border-bottom:0}
              .emp-list{padding:10px;border-bottom:solid 1px #eee}
               ")
  ),
 
  
  fluidRow(
    
    column(width = 10, offset = 1, 
            
           fluidRow(style="padding:30px 0;",
             column(width = 12, 
                   inputPanel(
                     selectInput("campus", "Select Campus",
                                 choices = list("Mintal" = 1, "Jacinto" = 2), selected = 1
                                 )
                   
                     
                   )),
                    
             
               column(width = 3,
                    h4(id="label_emp","Employees"),
                    tags$ul(id="emp_list",
                      uiOutput("list")
                    )
                     
                      
               ),
               column(width = 9,
                      
                  plotOutput("plot")
                    
                       
                      
               ),column(width = 9, offset =3,style="padding-top:20px;",
                     
                    column(width = 3,offset=3,
                           tags$div(
                             style = "text-align:center;border:solid 1px #ddd;padding:5px;",
                             "Tardy",h4(class="text-center","Yes"))
                            
                           ),
                    column(width = 3,
                           tags$div(
                             style = "text-align:center;border:solid 1px #ddd;padding:5px",
                             "Probability",h4(class="text-center","85%"))
                           
                    )
                        
                        
               )

           )

      )
  )
  
  
)

server <- function(input, output) {
 
  output$plot<- renderPlot({
   
      nb <- naive_bayes(Species ~ .,  iris)
     
      plot(nb, ask = FALSE,
           arg.num = list(main = "Naive Bayes Plot"))
      
       
    
  })
  sample.int(3 , 100 , replace = T)
  onevent("change", "campus", 
          output$lb <- renderText(input$campus),
          output$list <- renderUI({
            print(input$campus)
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
                                      "data-status"=employees[i,c("Marital Status")], 
                                      "data-years" = years,
                                      "data-education" = employees[i, c("education")],
                                      "data-tenure" = employees[i, c("tenure")],
                                      "data-department" = departmentName[1,c("name")],
                                      "data-age" = age
                                      
              ))
              
            }       
            return(LL)
          })
  )
  
  p<- function(x, session, inputname) {
    print(x["status"]);
  }
  removeInputHandler("s")
  registerInputHandler("s", p)
  
  
  shinyjs::runjs(
    '
      $("#emp_list").on("click",".li",function(){
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


  