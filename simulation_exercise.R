#  Simulation Exercise : 1

set.seed(10000)
mu<-3
phi1<-0.3
phi2<-0.6
zeta=1
alpha1=0.8
y=c()
y[2]=y[1]=0
v=rnorm(1000,mean=0,sd=1)
sigma_squared=c()
sigma_squared[1]=0.1

# generating the time series
for (t in 2:(length(v)-2)){
sigma_squared[t]= zeta + alpha1*(sigma_squared[t-1]*v[t-1]*v[t-1])
sigma_squared=append(sigma_squared,sigma_squared[t])
}



for (t in 3: (length(v)-1)){
y[t]<- mu + phi1*y[t-1] +phi2*y[t-2]+ sqrt(sigma_squared[t])*v[t]
y=append(y,y[t]) 
}
######Train&Test Split#############
n <- length(y)
n.train <- floor(n*0.90) 
n.test <- n-n.train

y_train<- y[1:n.train]
y_test<-tail(y,n.test)

library(forecast)
#plot of y_train & y_test vs t
plot(ts(y_train))
plot(ts(y_test))

# fitting arima model using auto.arima
mod1=auto.arima(ts(y_train))
summary(mod1)

predicted= forecast(mod1,100)
autoplot(predicted)
# ut plot
residual= y_test-predicted$mean
plot(residual)
#ut^2 plot

plot(residual^2)

#ut^2 vs u(t-1)^2 plot
ut_squared<-residual^2
plot(ut_squared[1:99],ut_squared[2:100])

## forecasting error for the test set
sqrt(sum((predicted$mean-y_test )^2)/n.test)
#Value = 3.909574

#Simulation Exercise : 2
mu<-3
phi1<-0.8
zeta=1
alpha1=0.5
delta1=0.3
z=c()
z[1]=0
w=rnorm(1000,mean=0,sd=1)
sigma_square=c()
sigma_square[1]=0.1
for (t in 2:(length(w)-2)){
  sigma_square[t]= zeta + alpha1*(sigma_square[t-1]*w[t-1]*w[t-1]) + delta1*sigma_square[t-1]
  sigma_square=append(sigma_square,sigma_square[t])
}
# generating the time series

for (t in 2: (length(w)-1)){
  z[t]<- mu + phi1*z[t-1] + sqrt(sigma_squared[t])*w[t]
  z=append(z,z[t]) 
}


######Train&Test Split#############
n <- length(z)
n.train <- floor(n*0.90) 
n.test <- n-n.train

z_train<- z[1:n.train]
z_test<-tail(z,n.test)

library(forecast)
#plot of y_train & y_test vs t
plot(ts(z_train))
plot(ts(z_test))

# fitting arima model using auto.arima
mod2=auto.arima(ts(z_train))
summary(mod2)

predicted_new= forecast(mod2,100)

autoplot(predicted_new)
# ut plot
residual_new= z_test-predicted_new$mean
plot(residual)
#ut^2 plot

plot(residual^2)

#ut^2 vs u(t-1)^2 plot
ut_squared<-residual^2
plot(ut_squared[1:99],ut_squared[2:100])

## forecasting error for the test set
sqrt(sum((predicted_new$mean-z_test )^2)/n.test)
#Value = 2.512857
