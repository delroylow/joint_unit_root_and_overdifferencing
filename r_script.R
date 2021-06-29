#Package
library(tseries)
library(tidyverse)
library(data.table)

#Monte Carlo Simulation 1

MC <- 1000 #MC Trial
r <- NULL #Empty Bernoulli Result Variable
phi <- c(0.85,seq(0.90,1,by=0.025))
sample_size <- seq(100,500,by = 100)

for(p in phi){
  for( T in sample_size){
    if(p < 1){
      for(i in 1:MC){
        
        series <- arima.sim(model=list(ar = p), n = T) 
        
        adf <- adf.test(series, alternative = "stationary")[["p.value"]]
        kpss <- kpss.test(series, null="Level")[["p.value"]]
        
        r <- rbind(tibble("Sample_Size" = T, "Phi" = p, "ADF_Result" = adf < 0.05, "KPSS_Result" = kpss > 0.05, "Join" = ifelse(adf <0.05 & kpss > 0.05, 1, 0)), r)
        
      }
    }else{
      for( i in 1:MC){
        
        series <- cumsum(rnorm(T,0,1))
        
        adf <- adf.test(series, alternative = "stationary")[["p.value"]]
        kpss <- kpss.test(series, null="Level")[["p.value"]]
        
        r <- rbind(tibble("Sample_Size" = T, "Phi" = p, "ADF_Result" = adf > 0.05, "KPSS_Result" = kpss < 0.05, "Join" = ifelse(adf <0.05 & kpss > 0.05, FALSE, TRUE)), r)
        
      }
    }
  }
}

#Monte Carlo Simulation 2

result <- NULL
MC <- 2500

ss <- c(100,500,1000)
phi <- c(.85,.90,.95,.99)

for(p in phi){
  for(t in ss){
    for(i in 1:2500){
      y <- arima.sim(model = list(ar = p), t)
      ds <- data.frame("Y" = diff(y), "X" = shift(diff(y), n = 1L, type = "lag"))
      result <- rbind(result, tibble("Sample_Size" = t, "True_Phi" = p, "Est_Phi"=lm(Y~X - 1, data=ds)$coefficients))
    }
  }
}
