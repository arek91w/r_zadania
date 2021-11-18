zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10) )
zbiorD

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

for(i in names(zbiorD)) {       # for-loop over columns
  if(typeof(unlist(zbiorD[i])) == 'integer' | typeof(unlist(zbiorD[i])) == 'double') {
    print(MinMax(zbiorD[i], 2, 4))
    print(max(zbiorD[i]))
  }
  else {
    zbiorD[i] <- zbiorD[i]
  }
}

x <- c(2, -1, 12, 44)
y <- MinMax(x, 0, 1)
print(y)

kNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) {
  
  # sprawdz czy X jest macierza lub ramka danych
  
  if (!(is.matrix(X) | is.data.frame(X)))  {
    print("INPUT ERROR: X musi być macierza lub ramka danych!")
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
    knn$X <- X
    knn$y <- y_tar
    knn$k <- k
    return(knn)
  }
}

y2 <- kNNtrain(emp.data, emp.data, 1, 2, 2)
y2

y <- c(1,2,3,NA)

NA %in% emp.data
is.na(emp.data)
sum(is.na(y))
match(TRUE, is.na(y))

A <- matrix(c(1:5))
A
!is.matrix(A)
      