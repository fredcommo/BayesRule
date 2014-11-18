require(mclust)

bayesRule <- function(N, Mu, V, P){
  mu1 <- Mu[1]; mu2 <- Mu[2]
  v1 <- V[1]; v2 <- V[2]
  p1 <- P[1]; p2 <- P[2]
  n1 <- N*p1; n2 <- N*p2
  num <- (n1-1)*v1 + (n2-1)*v2
  denom <- (n1+n2-2)
  Vhat <- num/denom
  C <- (mu1+mu2)/2 + Vhat/(mu1 - mu2)*log(p2/p1)
  return(C)
}
dLines <- function(N, Mu, V, P){
  nk <- length(Mu)
  for(ii in 1:nk){
    x <- sort(rnorm(1e4, Mu[ii], sqrt(V[ii])))
    d <- dnorm(x, Mu[ii], sqrt(V[ii]))
    d <- d*P[ii]
    lines(x, d, col = rgb(ii/nk, 0.2, (nk-ii)/nk, 0.75))
    polygon(x, d, col = rgb(ii/nk, 0.2, (nk-ii)/nk, .25))    
  }
}
