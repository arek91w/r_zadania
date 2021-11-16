add_two_numbers <- function(x, y) {
  z <- x + y
  return(z)
}

z <- add_two_numbers(2, 3)
print(z)
emp.data <- -3
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,NA ,611.0,729.0,0), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)

kNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) {
  # sprawdz czy X jest macierza lub ramka danych
  
  if (!(is.matrix(X) | is.data.frame(X)))  {
    print("INPUT ERROR: X musi być macierza lub ramka danych!")
  }
  
  # sprawdz czy X i y_tar nie maja brakow danych
  
  if ((sum(is.na(X))+sum(is.na(y_tar))) > 0)  {
    print("INPUT ERROR: Uzupelnij braki danych!")
  }
  
  # sprawdz czy k jest wieksze od zera
  
  if(k <= 0)  {
    print("INPUT ERROR: k musi byc wieksze od zera!")
  }
  else {
    return(0)
  }
}

kNNtrain(emp.data, emp.data, 1, 2, 2)


y <- c(1,2,3,NA)

NA %in% emp.data
is.na(emp.data)
sum(is.na(y))
match(TRUE, is.na(y))

A <- matrix(c(1:5))
A
!is.matrix(A)
      