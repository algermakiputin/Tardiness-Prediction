library(RMySQL)
library(dplyr)
sqlQuery <- function (query) {
  
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(), user="root", password='', dbname='hris', host='localhost')
  # close db connection after function call exits
  on.exit(dbDisconnect(DB))
  
  # send Query to btain result set
  rs <- dbSendQuery(DB, query)
  
  result <- fetch(rs, -1)
  
  return(result)
  
}


employees<- sqlQuery("SELECT * FROM employees WHERE employment_type = 1")
 
datasets = data.frame(
  "years" = character(0),
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
    departmentName = sqlQuery(paste("SELECT name FROM departments WHERE id=", x['department_id']))
    
    late = "no"
    isLate = (strptime(timeIn, "%H:%M") > strptime(startTime, "%H:%M"));
    
    if (isLate == TRUE) {
      late = "yes";
    }
    
    datasets<<- rbind(datasets, data.frame("years" = 1,
                                          "marital_status"= x['marital_status'],
                                          'age' = 21,
                                          'education' = x['education'],
                                          'tenure' = x['tenure'],
                                          'department' = departmentName['name'],
                                          'late' = late))
  })
  
}) 

 



 







