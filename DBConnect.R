library(RMySQL)
library(DBI)
library(e1071)
library(naivebayes)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(lubridate)

sqlQuery <- function (query) {
  DB <- dbConnect(MySQL(), user="root", password='', dbname='hris', host='localhost')
  
  on.exit(dbDisconnect(DB))
  rs <- dbSendQuery(DB, query)
  
  result <- fetch(rs, -1)
  
  return(result)
  
}

getDataset <- function(campus) {
  employees<- sqlQuery(paste("SELECT employees.*, departments.name as departmentName FROM employees 
                             INNER JOIN departments ON employees.department_id = departments.id 
                             WHERE employees.employment_type = '1'
                             AND employees.campus_id=", campus))
  
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
      
      attendances = sqlQuery(paste("SELECT min(date) as date FROM attendances WHERE DATE_FORMAT(date, '%Y') = YEAR(CURRENT_TIMESTAMP) AND campus_id='", campusID, "' AND employee_id ='", employeeID,"' GROUP BY DATE_FORMAT(date,'%Y-%m-%d')"))
     
      if (nrow(attendances) != 0) {
        apply(attendances,1,function(a,b) {
          dayOfWeek = as.character(wday(a['date'], week_start = 1));
          if (is.na(dayOfWeek)) {
            dayOfWeek = 1;
          }
          schedules = sqlQuery(paste("SELECT min(start) as start FROM schedules WHERE employee_id=", employeeID, ' AND campus_id=', campusID, ' AND day =', dayOfWeek))
          if (!is.na(schedules)) {
            startTime = schedules['start']
            
            timeIn = strftime(a['date'],"%H:%M:%S")
            
            late = "no"
            isLate = (strptime(timeIn, "%H:%M") > strptime(startTime, "%H:%M"));
            
            if (isTRUE(isLate)) {
              late = "yes"
            }
            
            
            if (!is.null(schedules)) {
              years = as.integer(floor(as.numeric( as.Date(Sys.Date()) - as.Date(x['date_joining'], "%Y-%m-%d") ) / 365.25))
              if (years < 0) {
                years = 0
              }
              
              if (as.integer(x['tenure']) == 1) {
                x['tenure'] = "Yes";
              }else {
                x['tenure'] = "No";
              }
              
              datasets<<- rbind(datasets, data.frame("years" = years,
                                                     "marital_status"= as.character(x['marital_status']),
                                                     'age' = as.integer(floor(as.numeric( as.Date(Sys.Date()) - as.Date(x['birthday'], "%Y-%m-%d") ) / 365.25)),
                                                     'education' = as.character(x['education']),
                                                     'tenure' = as.character(x['tenure']),
                                                     'department' = as.character(x['departmentName']),
                                                     'late' = as.character(late)))
            }
          }
        })
      }
      
    }) 
    data <- datasets
   
    return(data)
  }else {
    return("")
  }
  
  
} 

