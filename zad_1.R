zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10) )
zbiorD
x2 = runif(10)
x2


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
t <- normalize(zbiorD, 2, 44)
t

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
    knn$X <- normalize(X, XminNew, XmaxNew)
    knn$y <- y_tar
    knn$k <- k
    return(knn)
  }
}

y2 <- kNNtrain(zbiorD, x2, 3, 2, 6)
y2

      