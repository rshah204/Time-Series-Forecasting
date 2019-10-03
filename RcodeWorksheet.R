rm(list=ls())

#Import libraries
library(nlme)
library(lmtest)
library(stats)
library(strucchange)
library(forecast)

## Input data
#Import raw data file
passenger=read.csv(file.choose(), header = TRUE)
attach(passenger)
View(passenger)

## Creating sime series object
data_title= "Monthly Totals of Airline Passengers-All Foreign Carriers"
data_ts=ts(TOTAL, start=c(2003,5), end=c(2014, 5), frequency = 12)
data_ts_full=ts(TOTAL, start=c(2003,5), end=c(2015, 8), frequency = 12)

## Start Analysis
# Plot the raw data
plot(data_ts, ylab="Monthly Passenger Totals", main=paste(c(data_title, "Training Data")), col="blue")
plot(data_ts_full, ylab="Monthly Passenger Totals", main=paste(c(data_title, "Full Data Set")), col="red")

## Plot Seasonal/Trend/Remainder to and see % of variance in each
data_stl=stl(data_ts, s.window = 12)
summary(data_stl)
plot(data_stl, main = paste(c("Seasonal Decomposition of Time Series", data_title)), col="blue")

monthplot(data_ts, labels = month.abb, ylab ='Monthly Passenger Totals', xlab='Month', main=paste(c("Month Plot", data_title, "Training Data")), col="blue")

#library(forecast)
seasonplot(data_ts, ylab ='Monthly Passenger Totals', main=paste(c("Season Plot", data_title, "Training Data")), col=rainbow(12))

trend = ts(seq(from = 1, to = 133, by = 1))
trend_full = ts(seq(from = 1, to = 148, by = 1))

trend2 = trend^2; trend3 = trend^3; trend4 = trend^4;

###########################################################################################
################### Determine Polynomial order of Deterministic part ######################
###########################################################################################

fit_poly1=gls(data_ts ~ trend)
rss_poly1=sum(resid(fit_poly1)^2)
summary(fit_poly1)

fit_poly2=gls(data_ts ~ trend + trend2)
rss_poly2=sum(resid(fit_poly2)^2)
summary(fit_poly2)
#F-test to determine significant model
F_2_1=((rss_poly1-rss_poly2)/1)/(rss_poly2/(133-length(fit_poly2$coefficients)))

fit_poly3=gls(data_ts ~ trend + trend2 + trend3)
rss_poly3=sum(resid(fit_poly3)^2)
summary(fit_poly3)
#F-test to determine significant model
F_3_2=((rss_poly2-rss_poly3)/1)/(rss_poly3/(133-length(fit_poly3$coefficients)))

fit_poly4=gls(data_ts ~ trend + trend2 + trend3 + trend4)
rss_poly4=sum(resid(fit_poly4)^2)
summary(fit_poly4)
#F-test to determine significant model
F_4_3=((rss_poly3-rss_poly4)/1)/(rss_poly4/(133-length(fit_poly4$coefficients)))
#Polynomial order 3 is adequate

write.table(data.frame(trend), "training_data.csv", col.names = TRUE, row.names = FALSE)
write.table(data.frame(data_ts), "training_data.csv", append=TRUE, col.names = TRUE, row.names = FALSE)
getwd()
#Change column name to Trend & Data_ts
data=read.csv(file.choose(), header=TRUE)
attach(data)

#Graphically showing Polynomial fitting on Raw Data
plot(Data_ts~Trend, ylab="Monthly Passenger Totals", xlab = "Month", main=paste(c("Polynomial fitting on Raw Data")), type="l")
lines(fitted(fit_poly1), col=2, lty=2, lwd=2)
lines(fitted(fit_poly2), col=3, lty=3, lwd=2)
lines(fitted(fit_poly3), col=4, lty=4, lwd=3)
lines(fitted(fit_poly4), col=5, lty=5, lwd=2)
legend("topleft",lty=1:5, pch=1, col=1:5, c("Raw Data","1st order Polynomial","2nd order Polynomial", "3rd order Polynomial", "4th order Polynomial"))


#########################################################################################
################### Determine Periodic order of Deterministic part ######################
#########################################################################################

#Plot of residuals from Polynomial fit
plot(resid(fit_poly3), ylab="Monthly Passenger Totals", xlab = "Time", main=paste(c("Plot of residuals after removing the polynomial trend from the deterministic model")), type="l", col="red")

sin_cos1 = cbind(sin(2*pi*trend/12), cos(2*pi*trend/12))
sin_cos2 = cbind(sin(2*pi*trend/12), cos(2*pi*trend/12), sin(2*pi*trend/6), cos(2*pi*trend/6))
sin_cos3 = cbind(sin(2*pi*trend/12), cos(2*pi*trend/12), sin(2*pi*trend/6), cos(2*pi*trend/6), sin(2*pi*trend/4), cos(2*pi*trend/4))
sin_cos4 = cbind(sin(2*pi*trend/12), cos(2*pi*trend/12), sin(2*pi*trend/6), cos(2*pi*trend/6), sin(2*pi*trend/4), cos(2*pi*trend/4), sin(2*pi*trend/3), cos(2*pi*trend/3))
sin_cos5 = cbind(sin(2*pi*trend/12), cos(2*pi*trend/12), sin(2*pi*trend/6), cos(2*pi*trend/6), sin(2*pi*trend/4), cos(2*pi*trend/4), sin(2*pi*trend/3), cos(2*pi*trend/3), sin(2*pi*trend/2.4), cos(2*pi*trend/2.4))
sin_cos6 = cbind(sin(2*pi*trend/12), cos(2*pi*trend/12), sin(2*pi*trend/6), cos(2*pi*trend/6), sin(2*pi*trend/4), cos(2*pi*trend/4), sin(2*pi*trend/3), cos(2*pi*trend/3), sin(2*pi*trend/2.4), cos(2*pi*trend/2.4), sin(2*pi*trend/2), cos(2*pi*trend/2))

fit_poly3_sc1 = gls(data_ts ~ trend + trend2 + trend3 + sin_cos1)
rss_poly3_sc1 = sum(resid(fit_poly3_sc1)^2)
summary(fit_poly3_sc1)

fit_poly3_sc2 = gls(data_ts ~ trend + trend2 + trend3 + sin_cos2)
rss_poly3_sc2 = sum(resid(fit_poly3_sc2)^2)
summary(fit_poly3_sc2)
#F-test to determine significant model
F_sc_2_1=((rss_poly3_sc1-rss_poly3_sc2)/2)/(rss_poly3_sc2/(133-length(fit_poly3_sc2$coefficients)))

fit_poly3_sc3 = gls(data_ts ~ trend + trend2 + trend3 + sin_cos3)
rss_poly3_sc3 = sum(resid(fit_poly3_sc3)^2)
summary(fit_poly3_sc3)
#F-test to determine significant model
F_sc_3_2=((rss_poly3_sc2-rss_poly3_sc3)/2)/(rss_poly3_sc3/(133-length(fit_poly3_sc3$coefficients)))

fit_poly3_sc4 = gls(data_ts ~ trend + trend2 + trend3 + sin_cos4)
rss_poly3_sc4 = sum(resid(fit_poly3_sc4)^2)
summary(fit_poly3_sc4)
#F-test to determine significant model
F_sc_4_3=((rss_poly3_sc3-rss_poly3_sc4)/2)/(rss_poly3_sc4/(133-length(fit_poly3_sc4$coefficients)))

fit_poly3_sc5 = gls(data_ts ~ trend + trend2 + trend3 + sin_cos5)
rss_poly3_sc5 = sum(resid(fit_poly3_sc5)^2)
summary(fit_poly3_sc5)
#F-test to determine significant model
F_sc_5_4=((rss_poly3_sc4-rss_poly3_sc5)/2)/(rss_poly3_sc5/(133-length(fit_poly3_sc5$coefficients)))

fit_poly3_sc6 = gls(data_ts ~ trend + trend2 + trend3 + sin_cos6)
rss_poly3_sc6 = sum(resid(fit_poly3_sc6)^2)
summary(fit_poly3_sc6)
#F-test to determine significant model
F_sc_6_5=((rss_poly3_sc5-rss_poly3_sc6)/2)/(rss_poly3_sc6/(133-length(fit_poly3_sc6$coefficients)))

# eliminate cos(2*pi*trend/12) and sin(2*pi*trend/6) since p value is high and get reduced model and check adequacy
sin_cos5.reduced = cbind(sin(2*pi*trend/12), cos(2*pi*trend/6), sin(2*pi*trend/4), cos(2*pi*trend/4), sin(2*pi*trend/3), cos(2*pi*trend/3), sin(2*pi*trend/2.4), cos(2*pi*trend/2.4))
fit_poly3_sc5.reduced = gls(data_ts ~ trend + trend2 + trend3 + sin_cos5.reduced)
rss_poly3_sc5.reduced = sum(resid(fit_poly3_sc5.reduced)^2)
summary(fit_poly3_sc5.reduced)
#F-test to determine significant model
F_sc_5_5r=((rss_poly3_sc5.reduced-rss_poly3_sc5)/2)/(rss_poly3_sc5/(133-length(fit_poly3_sc5$coefficients)))
#fit_poly3_sc5.reduced is the optimal model, no other coefficients insignificant


##################################################################################
##################### Modeling procedure of Stochastic Part ######################
##################################################################################

#Get the reiduals of the adequate(optimal) deterministic model so as to get the stochastic part in raw data and proceed to ARMA modeling
trend_residual = resid(fit_poly3_sc5.reduced)
trend_fitted = fitted(fit_poly3_sc5.reduced)

#Comparison of the deterministic trend and actual data
plot(Data_ts~Trend, ylab="Monthly Passenger Totals", xlab = "Month", main=paste(c("Comparison of the Deterministic trend and Raw (actual) Data")), type="p")
lines(trend_fitted, col="blue", lwd=2)

#Plot of residuals of the deterministic model
plot(trend_residual, ylab = "Residuals in Monthly Passenger Totals", xlab = "Time",  main = paste(c("Plot of residuals of the deterministic model")), col="red")
abline(h=0, col="black", lty="dotted")

## Plot acf and pcf of residuals of the deterministic model
par(mfrow=c(1,2))
acf(trend_residual, col="red", main=paste(c(data_title, " Plot of autocorrelations of residuals from the", "deterministic model"))) 
pacf(trend_residual, col="blue", main=paste(c(data_title, " Plot of partial autocorrelations of residuals from the", "deterministic model")))
par(mfrow=c(1,1))
#Checking stationarity of residuals
adf.test(trend_residual, alternative = "stationary") #Augmented Dickey-Fuller, Alternative hypothesis: stationary

##ARMA modeling on the residuals obtained from Deterministic model
#ARMA(2,1)
arima_201 = arima(trend_residual, order = c(2,0,1))
arima_201
rss_201 = sum(resid(arima_201)^2)

#ARMA(4,3)
arima_403 = arima(trend_residual, order = c(4,0,3))
arima_403
rss_403 = sum(resid(arima_403)^2)
#F-test to determine significant model
F_stat1 = ((rss_201 - rss_403)/4)/(rss_403/(133 - length(arima_403$coef)))

#ARMA(6,5)
arima_605 = arima(trend_residual, order = c(6,0,5))
arima_605
rss_605 = sum(resid(arima_605)^2)
#F-test to determine significant model
F_stat2 = ((rss_403 - rss_605)/4)/(rss_605/(133 - length(arima_605$coef)))
#ARMA(4,3) is significant

#CI(95%) of ar4 and ma3 from ARMA(4,3) includes zero, test this here. Check significance between ARMA(3,2) and ARMA(4,3)
arima_302 = arima(trend_residual, order = c(3,0,2))
arima_302
rss_302 = sum(resid(arima_302)^2)
#F-test to determine significant model
F_stat3 = ((rss_302 - rss_403)/2)/(rss_403/(133 - length(arima_403$coef)))
#ARMA(3,2) is significant

#CI(95%) of ar3 of ARMA(3, 2) includes zero, test this here. Check significance between ARMA(2,2) and ARMA(3,2)
arima_202 = arima(trend_residual, order = c(2,0,2))
arima_202
rss_202 = sum(resid(arima_202)^2)
#F-test to determine significant model
F_stat4 = ((rss_202 - rss_302)/1)/(rss_302/(133 - length(arima_302$coef)))

# ARMA(2,2) is adequate ARMA model for the stochastic part
# Choose ARMA(2,2)

#######################################################################################
################################## Joint Optimization #################################
#######################################################################################

# Fit Integrated trend + ARIMA model
trend_regressors = cbind(trend, trend2, trend3, sin(2*pi*trend/12), cos(2*pi*trend/6), sin(2*pi*trend/4), cos(2*pi*trend/4), sin(2*pi*trend/3), cos(2*pi*trend/3), sin(2*pi*trend/2.4), cos(2*pi*trend/2.4))
integrated_model = arima(data_ts, order=c(2,0,2), xreg=trend_regressors)
integrated_model

## Print coefs, std errors and calculate p-values for each of the coefficients
variable_matrix = data.frame(estimate=integrated_model$coef,
                             std_err=rep(0,length(integrated_model$coef)), p_value=rep(0,length(integrated_model$coef)))

for(i in 1:length(integrated_model$coef))
{
  variable_matrix[i,2] = sqrt(integrated_model$var.coef[i,i])
  variable_matrix[i,3] = dt(integrated_model$coef[i]/sqrt(integrated_model$var.coef[i,i]),
                            length(data_ts)-length(integrated_model$coef))
}
variable_matrix

##Stability of jointly estimated ARMA(2,2) model
#Calculated Green's Functions by hand upto j=30
GreenFunc = read.csv(file.choose(), header=TRUE)
attach(GreenFunc)
plot(Gj~j, type="l", main=paste(c("Green's functions for ARMA(2,2) model")), col="red", lwd=2)
abline(h=0, lty="dashed")
abline(v=0, lty="dashed")
#Inverse AR and inverse MA roots
plot(integrated_model)

#Comparison of Integrated Model and Raw data
plot(data_ts, ylab="Monthly Passenger Totals", type="p", main=paste(c("Comparison of the Integrated Model and Raw (actual) Data")))
lines(fitted(integrated_model), col="blue", lwd=2)
#integrated_model residual plots
plot(integrated_model$residuals, ylab="Monthly Totals", col="red", type="p", main=paste(c("Integrated Model Residuals","Plot of residuals from the ARMA(2,2) and the deterministic model")))
abline(h=0, col="black", lty="dotted")
par(mfrow=c(1,2))
acf(integrated_model$residuals, main=paste(c("Plot of autocorrelations of residuals from", "the ARMA(2,2) and the deterministic model")), col="red")
pacf(integrated_model$residuals, main=paste(c("Plot of partial autocorrelations of residuals from", "the ARMA(2,2) and the deterministic model")), col="blue")
par(mfrow=c(1,1))

##Diagnostics on residuals
#the Box-Pierce or Ljung-Box test statistic for examining the null hypothesis of independence
Box.test(integrated_model$residuals, 5, type="Box-Pierce") #correlated residuals?
Box.test(integrated_model$residuals, 5, type="Ljung") #correlated residuals?
shapiro.test(integrated_model$residuals) #normality of residuals?

plotResidualHist <- function(Model_Residuals) #Checking normality with Histogram Plot
{
  # make a histogram of the model residuals:
  mybinsize <- IQR(Model_Residuals)/4
  mysd   <- sd(Model_Residuals)
  mymin  <- min(Model_Residuals) - mysd*5
  mymax  <- max(Model_Residuals) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the model residuals, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(Model_Residuals, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of model residuals:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotResidualHist(integrated_model$residuals)


#########################################################################################
################################## Forecasting ##########################################
#########################################################################################

#Get the range of prediction period
test_range = c((length(data_ts)+1): length(data_ts_full))

test_xreg = cbind(trend_full[test_range], trend_full[test_range]^2, trend_full[test_range]^3, sin(2*pi*trend_full[test_range]/12), cos(2*pi*trend_full[test_range]/6), sin(2*pi*trend_full[test_range]/4), cos(2*pi*trend_full[test_range]/4), sin(2*pi*trend_full[test_range]/3), cos(2*pi*trend_full[test_range]/3), sin(2*pi*trend_full[test_range]/2.4), cos(2*pi*trend_full[test_range]/2.4))

#Predict
joint_pred = predict(integrated_model, n.ahead=15, newxreg=test_xreg)
#Define upper and lower limits on forecasts
U = joint_pred$pred + 1.96*joint_pred$se
L = joint_pred$pred - 1.96*joint_pred$se
#U1 = joint_pred$pred + 2.58*joint_pred$se
#L1 = joint_pred$pred - 2.58*joint_pred$se

par(mfrow=c(1,1))
ts.plot(data_ts_full, joint_pred$pred, col=c("red","blue"), ylab="Monthly Passenger Totals", main=paste(c(data_title, "vs Forecast with 95% Confidence Interval")))
lines(U, col="dark green", lty="dashed")
lines(L, col="dark green", lty="dashed")
#lines(U1, col="dark green", lty="dashed")
#lines(L1, col="dark green", lty="dashed")
abline(v=2014.4, col="black", lty="dotted")
legend("topleft", lty=c(1,1,2), col=c(2,4,3), c("Raw Data","Forecast","Confidence Interval"))

#joint_pred=forecast.Arima(integrated_model, xreg=test_xreg)
#plot(joint_pred)
#joint_pred

#close-up on forecast period
closeup_window=window(data_ts_full, start=2014.2)
plot(closeup_window, ylab="Monthly Passenger Totals", main=paste(c(data_title, "vs Forecast with 95% Confidence Interval")), col="red", ylim=c(6e+06, 10.5e+06))
lines(joint_pred$pred, col="blue", lwd=2)
lines(U, col="dark green", lty="dashed")
lines(L, col="dark green", lty="dashed")
lines(U1, col="dark green", lty="dashed")
lines(L1, col="dark green", lty="dashed")
abline(v=2014.4, col="black", lty="dotted")

#Holt Winters Model
hwfit=window(data_ts, start=c(2003,05))
#hwfit1=hw(hwfit, seasonal = "additive")
hwfit2=hw(hwfit, seasonal = "multiplicative")
plot(hwfit2, ylab= "Monthly Passenger Totals", plot.conf=FALSE, type="l", fcol="white", xlab="Time")
#lines(fitted(hwfit1), col="green", lty=2)
lines(fitted(hwfit2), col="red", lty=2)
#lines(hwfit1$mean, type="o", col="green")
lines(hwfit2$mean, type="o", col="red")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Multiplicative","Holt Winters' Additive"))
lines(data_ts_test, col="blue", lty="dashed")
hw2resid=resid(hwfit2)
plot(hw2resid)
summary(hwfit2)
acf(hw2resid)

# Exponential Model
fo=TOTAL~R1*exp(r1*Year1)
exp1=nls(fo, data= passenger, start=list(R1=4191039, r1=.00574713), trace = TRUE)
