zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10) )
zbiorD
d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

a <- c("A", "B", "B")
b <- c("B", "B", "B")


d_hamming <- function( x_i, x_n ){
  return( sum(x_i != x_n) )
}

print(d_hamming(a, b))
y2

y2$y <- c("A", "A", "B", "C", "C", "C", "A", "A", "B", "B")
y2$y <- as.factor(y2$y)
y2$k <- 4
print(is.factor(y2$y))

dEuklides <- function(dane){
  n <- nrow( dane )
  odl <- matrix( 0, n, n )
  for( i in 1:n ){
    for( j in i:n ){
      odl[ j, i ] <- d_euklides( dane[i,], dane[j,] )
    }
  }
  return( odl )
}

dEuklides(zbiorD)
dist( zbiorD )


KNNpred <- function( model, X ){
  
  # sprawdzenie czy kolumny sie zgadzaja
  
  if(ncol(model$X) != ncol(X)) {
    print(ncol(model$X))
    print(ncol(X))
    print("rozna ilosc kolumn!")
  }
  for(i in names(model$X)) {
    print(i)
  }
  for(i in names(X)) {
    print(i)
  }
  
  if (is.numeric(model$y)) {
    nTrain <- nrow( model$X )
    nPred <- nrow( X )
    odl <- matrix( 0, nTrain, nPred )
    for( i in 1:nTrain ){
      for( j in 1:nPred ){
        odl[i,j] <- d_euklides( model$X[i,], X[j,] )
      }
    }
    pred <- double( nPred )
    for( i in 1:nPred ){
      kNaj <- order( odl[,i] )
      kNaj <- kNaj[1:model$k]
      y_hat <- mean( model$y[ kNaj ] )
      pred[ i ] <- y_hat
    }
    return( pred )
  }
  else if (is.factor(model$y)) {
    print("zadanie klasyfikacji")
    nTrain <- nrow( model$X )
    nPred <- nrow( X )
    odl <- matrix( 0, nTrain, nPred )
    for( i in 1:nTrain ){
      for( j in 1:nPred ){
        odl[i,j] <- d_euklides( model$X[i,], X[j,] )
      }
    }
    pred <- character( nPred )
    print(pred)
    for( i in 1:nPred ){
      kNaj <- order( odl[,i] )
      kNaj <- kNaj[1:model$k]
      y_hat <- model$y[ kNaj ]
      for( z in 1:length(y_hat )) {
        print(y_hat[z])
      }
      pred[i] <- as.character(y_hat[1])
    }
    return( pred )
  }
}

x2
nrow(x2)
KNNpred( y2, y2$X )

tt <- as.numeric(y2$y)
typeof(y2$y)
library(caret)
KNNreg_pakiet <- knnreg( y2$X, y2$y, k = 3 )
predict( KNNreg_pakiet, y2$X)
