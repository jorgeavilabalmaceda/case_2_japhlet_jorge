install.packages("sandwich")
library(sandwich)
set.seed(123)
sample_size<-100
number_of_iterations<-1000
critical_value<-1.96
reject_sigma2<-numeric(number_of_iterations)
reject_newey_west<-numeric(number_of_iterations)
reject_closed_form<-numeric(number_of_iterations)
for(iteration in 1:number_of_iterations){
  #Simulate y_t=1+u_t with MA(1) errors, phi=0.5
  y<-1+arima.sim(model=list(ma=0.5),n=sample_size)
  #OLS on a constant only 
  model<-lm(y~1)
  c_hat<-coef(model)
  u_hat<-residuals(model)
  #(i) short run variance
  var_sigma2<-mean(u_hat^2)/sample_size
  #(ii) Newey-West with 4 lags
  var_newey_west<-NeweyWest(model,lag=4,prewhite=FALSE)[1,1]
  #(iii) closed form from exercise 1(e)
  gamma_0<-sum(u_hat^2)/sample_size
  gamma_1<-sum(u_hat[-1]*u_hat[-sample_size])/(sample_size-1)
  var_closed_form<-(gamma_0+2*(1-1/sample_size)*gamma_1)/sample_size
  #t-tests for H0: c=1
  reject_sigma2[iteration]<-abs((c_hat-1)/sqrt(var_sigma2))>critical_value
  reject_newey_west[iteration]<-abs((c_hat-1)/sqrt(var_newey_west))>critical_value
  reject_closed_form[iteration]<-abs((c_hat-1)/sqrt(var_closed_form))>critical_value
}
#Empirical size
mean(reject_sigma2)
mean(reject_newey_west)
mean(reject_closed_form)
#Results:
#(i) around 0.14
#(ii) close to 0.05
#(iii) close to 0.05