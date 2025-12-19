compute_signal_metrics <- function(a, b, c, d) {
  if (any(c(a,b,c,d) == 0)) {
    return(data.frame(
      a=a,b=b,c=c,d=d,
      ROR=NA, ROR_L=NA, ROR_U=NA,
      PRR=NA, Chi2=NA
    ))
  }
  
  ROR <- (a/b)/(c/d)
  se  <- sqrt(1/a + 1/b + 1/c + 1/d)
  ROR_L <- exp(log(ROR) - 1.96*se)
  ROR_U <- exp(log(ROR) + 1.96*se)
  
  PRR <- (a/(a+b))/(c/(c+d))
  
  chi2 <- ( (a*d - b*c)^2 * (a+b+c+d) ) /
    ( (a+b)*(c+d)*(a+c)*(b+d) )
  
  data.frame(
    a=a,b=b,c=c,d=d,
    ROR=ROR,
    ROR_L=ROR_L,
    ROR_U=ROR_U,
    PRR=PRR,
    Chi2=chi2
  )
}
