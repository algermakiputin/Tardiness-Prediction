library(RMySQL)
library(DBI)
library(e1071)
library(naivebayes)
library(shiny)
library(shinycssloaders)
library(shinyjs)


sqlQuery <- function (query) {

  DB <- dbConnect(MySQL(), user="bb5079f5cfb4a6", password='5002d268', dbname='heroku_298d50bdf7908a3', host='us-cdbr-iron-east-01.cleardb.net',port=3306)
  on.exit(dbDisconnect(DB))
  rs <- dbSendQuery(DB, query)
  
  result <- fetch(rs, -1)
  
  return(result)
  
}

getDataset <- function(campus) {
  employees<- sqlQuery(paste("SELECT employees.*, departments.name as departmentName FROM employees 
                             INNER JOIN departments ON employees.department_id = departments.id 
                             WHERE employees.employment_type = 1 AND employees.campus_id=", campus))
 
  if ( nrow( employees ) ) {
    datasets = data.frame(
      "years" = integer(0),
      "marital_status"= character(0),
      'age' = integer(0),
      'education' = character(0),
      'tenure' = character(0),
      'department' = character(0),
      'late' = character(0));
    
    counter<- 0;
    
    apply(employees, 1, function(x,s){
      campusID = x['campus_id'];
      employeeID = x['employee_id'];
      scheduleID = x['schedule_id'];
      attendances = sqlQuery(paste("SELECT min(date) as date FROM attendances WHERE campus_id='", campusID, "' AND employee_id ='", employeeID,"' GROUP BY DATE_FORMAT(date,'%Y-%m-%d')"))
      schedules = sqlQuery(paste("SELECT * FROM schedules WHERE id=", scheduleID))
      
      apply(attendances,1,function(a,b) {
        startTime = schedules['start']
        endTime = schedules['end']
        timeIn = strftime(a['date'],"%H:%M:%S")
        late = "no"
        isLate = (strptime(timeIn, "%H:%M") > strptime(startTime, "%H:%M"));
        
        if (isTRUE(isLate)) {
          late = "yes"
        }
        
        datasets<<- rbind(datasets, data.frame("years" = as.integer(floor(as.numeric( as.Date(Sys.Date()) - as.Date(x['date_joining'], "%Y-%m-%d") ) / 365.25)),
                                               "marital_status"= as.character(x['marital_status']),
                                               'age' = as.integer(floor(as.numeric( as.Date(Sys.Date()) - as.Date(x['birthday'], "%Y-%m-%d") ) / 365.25)),
                                               'education' = as.character(x['education']),
                                               'tenure' = as.character(x['tenure']),
                                               'department' = as.character(x['departmentName']),
                                               'late' = as.character(late)))
      })
      
    }) 
    data <- datasets
   
    return(data)
  }else {
    return("")
  }
  
  
}
 
