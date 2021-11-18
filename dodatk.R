zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10) )
zbiorD
x2 = runif(10)
x2
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,12 ,611.0,729.0,0), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)

emp.data


MinMax <- function( x, new_min, new_max){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}


normalize <- function( df, new_min, new_max){
  for(i in names(df)) {       # for-loop over columns
    if(typeof(unlist(df[i])) == 'integer' | typeof(unlist(df[i])) == 'double') {
      df[i] <- MinMax(df[i], new_min, new_max)
    }
    else {
      df[i] <- df[i]
    }
  }
  return(df)
}

new_zbiorD <- normalize(zbiorD, 1, 3)

new_zbiorD


x <- c(2, -1, 12, 44)
y <- MinMax(x, 0, 1)
print(y)

kNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) {
  
  # sprawdz czy X jest macierza lub ramka danych
  
  if (!(is.matrix(X) | is.data.frame(X)))  {
    print("INPUT ERROR: X musi byÄ‡ macierza lub ramka danych!")
  }
  
  # sprawdz czy X i y_tar nie maja brakow danych
  
  else if ((sum(is.na(X))+sum(is.na(y_tar))) > 0)  {
    print("INPUT ERROR: Uzupelnij braki danych!")
  }
  
  # sprawdz czy k jest wieksze od zera
  
  else if(k <= 0)  {
    print("INPUT ERROR: k musi byc wieksze od zera!")
  }
  else {
    knn <- list()
    knn$X <- normalize(X, XminNew, XmaxNew)
    knn$y <- y_tar
    knn$k <- k
    return(knn)
  }
}

y2 <- kNNtrain(zbiorD, x2, 4, 2, 6)
y2

y <- c(1,2,3,NA)

NA %in% emp.data
is.na(emp.data)
sum(is.na(y))
match(TRUE, is.na(y))

A <- matrix(c(1:5))
A
!is.matrix(A)
