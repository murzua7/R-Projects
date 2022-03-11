#R Basics

#dataset

attach(Eth)
variable.names(Eth)

summary(Eth)
mean(Eth$low)
median(Eth$low)
sd(Eth$low)
quantile(Eth$low,0.25)

low_data = as.data.frame(Eth$low)
summary(closing_data)

typeof(Eth)

plot(Eth$low,type='l',lty = 1, col="pink")
boxplot(Eth$low)
hist(Eth$low)

#descriptive statistics for variables

as.double(low)
summary(low)

as.double(high)
summary(high)

as.double(volume)
summary(volume)

as.double(open)
summary(open)

as.double(close)
summary(close)

#github function

devtools::install_github("ekstroem/dataReporter")
library("dataReporter")
makeDataReport(Eth)


#creating the function

library(xts)
library(zoo)

tm <- as.POSIXct(paste(Eth$time), format="%Y-%m-%d %H:%M")

vol_ts <- xts(Eth$volume, tm)
    
TT = time(vol_ts)

trading_vol <- function(x,y,start,end)
{
  if(start > end)
  {
    print('ERROR: End year should be after start year')
    rtrn = NA
  }
  else
  {
  t1 = x[y==start]
  t2 = x[y==end]
  
  interval = t1:t2
  
  avrg = mean(interval)
  standard_d = sd(interval)
  
  coef_of_var = (standard_d/avrg)
  
  textflag = cat("The Coefficient of Variation is ",coef_of_var)
  
  list_return = list(avrg,standard_d,textflag)
  rtn = return(list_return)
  return(list_return)
  }
}

trading_vol(vol_ts,TT,"2021-12-01 00:35:00","2021-12-09 09:35:00")

#equation

N = 10000 # Sample size
B = 10000 # Number of iterations
set.seed(1)

yt = 0.7 + 0.3 * xt + 0.6 * zt + et

xt=rbeta(N,5,1)
zt=rnorm(N,mean = 10,sd=1)
et=rnorm(N,mean = 10,sd=10)

#derive yt

print(yt)
print(mean(yt))

#linear regression

linear_regression <- lm(yt~xt+zt)
summary(linear_regression)

#boostrap

bootstrap_coeff = foreach(b=1:B,.combine = 'cbind') %dopar% {
  set.seed(b)
  indices<-sample(N,N,replace = T)
  x1 = xt[indices]
  y1 = yt[indices]
  z1 = zt[indices]
  fit<-lm(y1~x1+z1)
  fit$coefficients
}

y = c(bootstrap_coeff[3,])
tth <-linear_regression$coefficients

plot(density(y),col='green')
abline(v=tth,col='black',lty=2)

#time estimate

library(doParallel)
n_cores <- detectCores() - 1
n_cores

library(tictoc)

inputs <- 1:200
tic('Using multiple cores')
results <- mclapply(inputs, y, mc.cores = n_cores)
toc()

tic('Without using cores')
results <- lapply(inputs, y)
toc()