# Normalizacja
MinMax <- function( x, x_min_new = 0, x_max_new = 1 ){
  ( x - min(x) ) / ( max(x) - min(x) ) *
  ( x_max_new - x_min_new ) + x_min_new
}

set.seed(123)
y <- rnorm( 100 )
summary( y )
summary( MinMax( y ) )
summary( MinMax( y, 5, 10 ) )

# Miary odleglosci

d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

set.seed(123)
zbiorD <- data.frame( x1 = c( rep(1,5),rep(2,5) ),
                      x2 = runif(10), x3 = rnorm(10) )

d_euklides( zbiorD[1,], zbiorD[1,] )
d_euklides( zbiorD[1,], zbiorD[2,] )

dist( x = zbiorD, method = "euclidean" )

DEuclidean <- function( D ){
  n <- nrow( D )
  wynik <- matrix( 0, n, n )
  for( i in 1:n ){
    for( j in i:n ){
      wynik[j,i] <- d_euklides( D[i,], D[j,] )
    }
  }
  return( wynik )
}
DEuclidean( zbiorD )
dist( x = zbiorD, method = "euclidean" )

# Algorytm K-NN
zbiorD <- data.frame( zbiorD, 
                      y_tar = ifelse( zbiorD$x1 == 1,
                                      zbiorD$x3 + 1,
                                      zbiorD$x3 + 10 ))
zbiorD

KNN_train <- function( X, y_tar, k = 5 ){
  knn <- list()
  knn$X <- X
  knn$y <- y_tar
  knn$k <- k
  return( knn )
}

KNNreg <- KNN_train( X = zbiorD[,-4], 
                     y_tar = zbiorD[,4], k = 5)
KNNreg

KNN_pred <- function( model, Xnew ){
  nTrain <- nrow( model$X )
  nPred <- nrow( Xnew )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[i,j] <- d_euklides( model$X[i,], Xnew[j,] )
    }
  }
  # print( odl )
  pred <- double( nPred )
  for( j in 1:nPred ){
    kNaj <- order( odl[,j] )
    # print(kNaj)
    kNaj <- kNaj[1:model$k]
    # print(kNaj)
    y_pred <- mean( model$y[kNaj] )
    # print( y_pred )
    pred[j] <- y_pred
  }
  return( pred )
}

KNN_pred( KNNreg, as.matrix( zbiorD[1:2,] ) )

install.packages( "caret" )
library( "caret" )




