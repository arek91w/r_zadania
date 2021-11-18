zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10) )
zbiorD
d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

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