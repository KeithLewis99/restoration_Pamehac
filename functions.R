spc <- function(x){
  #browser()
  for(i in seq_along(x$Sweep)){
    if(x$Sweep[i] == 1){
      x$spc[i] <- 0
    } else if(x$Sweep[i] == 2) {
      #  z[i] <- cumsum(x[i-1])
      x$spc[i] <- x$abun[i-1]
    } else {
#      x$spc[i] <- sum(x$abun[(i-2):(i-1)])
      x$spc[i] <- sum(x$abun[(i-(i-1)):(i-1)])
    }
    }
  return(x)     
}




# Seber Goodness of Fit test for three passes
cs_gf <- function(N, p, c1, c2, c3){
  (c1 - (N*p))^2/N*p + 
    (c2 - N*(1-p)*p)^2/(N*(1-p)*p) +
    (c3 - N*(1-p)^2*p)^2/(N*(1-p)^2*p)
}


spc1 <- function(var){
  #browser()
  for(i in seq_along(x$var)){
    if(x$var[i] == 1){
      x$spc[i] <- 0
    }
  }
  return(x)     
}
