spc <- function(x){
  #browser()
    for(i in seq_along(x$Sweep)){
    if(x$Sweep[i] == 1){
      x$spc[i] <- 0
    } else if(x$Sweep[i] == 2) {
      #  z[i] <- cumsum(x[i-1])
      x$spc[i] <- x$abun[i-1]
    } else {
      x$spc[i] <- sum(x$abun[(i-2):(i-1)])
    }
    }
  return(x)     
}
