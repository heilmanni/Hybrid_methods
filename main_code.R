#By using this file we can forecast in three different ways: with an ARX, an ANN and a hibrid methology

#necessary packages
require(forecast)
require(ModelMetrics)
require(gets)
require(readxl)
require(stats)
require(graphics)

#in ordet to have always the same results:
set.seed(2365)

#IMPORT TIMESERIES
#importing phil_data.R file
phil_data <- readxl::read_xlsx("C:/Users/Istvan/Documents/EFRP_D/Phil_data.xlsx", col_names = T)

#the output variable is the inflation; there is two options, based on GDP deflator and PCE deflator
y_GDP <- phil_data[,2]
y_GDP <- y_GDP[[1]]
y_PCE <- phil_data[,3]
y_PCE <- y_PCE[[1]]

#the regressors are the columns from 3 up to 9
x_reg <- phil_data[,4:9]
x_reg <- cbind(x_reg[[1]], x_reg[[2]], x_reg[[3]], x_reg[[4]], x_reg[[5]], x_reg[[6]])

#1. ARX MODEL
#let's see an average regression model
glm(y_GDP ~ x_reg)
summary(glm(y_GDP ~ x_reg))

#Ljung-Box autocorrelation tests
stats::Box.test(x = resid(glm(y_GDP ~ x_reg)), type = "Ljung")

glm(y_PCE ~ x_reg)
summary(glm(y_PCE ~ x_reg))
stats::Box.test(x = resid(glm(y_PCE ~ x_reg)), type = "Ljung")
#autocorrelation is presented in both cases

#sourcing arx_forecast function
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/arx_forecast.R")


arx_GDP <- arx_forecast(lisd = 230, losd = 13, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE <- arx_forecast(lisd = 230, losd = 13, y = y_PCE, xreg = x_reg, max_ar = 5)

#the best model is that where the MSE is the smallest and the p-value of the Ljung-Box test are larger than 0.05
#in both cases this is when phi = 1

#MSE = 0.1295500

gets::arx(y = y_GDP, ar = 1, mxreg = x_reg, mc = T)
gets::arx(y = y_PCE, ar = 1, mxreg = x_reg, mc = T)


#NEURAL NETWORKS METHODOLOGY
#sourcing ann_forecast function
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/ann_forecast.R")

model_ann <- ann_forecast(ltrain = 200, ltest = 30, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE <- ann_forecast(ltrain = 200, ltest = 30, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann[which.min(model_ann[,3]),]
#now dim = 1, nodes = 4
model_ann_PCE[which.min(model_ann_PCE[,3]),]
#now dim = 1, nodes = 4

#for comparing a forecast and an error value must be given for the out-of-sample data
#using ann_out_forecast function
ann_GDP <- ann_out_forecast(lisd = 230, losd = 13, dim = 1, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.1562852
ann_PCE <- ann_out_forecast(lisd = 230, losd = 13, dim = 1, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.1759325


#HYBRID METHODOLOGY
#sourcing hybrid_forecast
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/hybrid_forecast.R")

hybrid_GDP <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 230, losd = 13, phi = 1, brake = 0.85, max_dim = 2, max_nodes = 2)
#MSE = 0.09614764
hybrid_PCE <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 230, losd = 13, phi = 1, brake = 0.85, max_dim = 2, max_nodes = 2)
#MSE = 0.1407057


#ANALYSIS
#these matrices give the predicted values of each methods
forecast_GDP <- as.matrix(rbind(arx_GDP[1,4:16], ann_GDP[2:14], hybrid_GDP[2:14], y_GDP[231:243]))
forecast_PCE <- as.matrix(rbind(arx_PCE[1,4:16], ann_PCE[2:14], hybrid_PCE[2:14], y_PCE[231:243]))

#these matrices give the residuals of each methods
res_GDP <- rbind(arx_GDP[1,4:16]-y_GDP[231:243], ann_GDP[2:14]-y_GDP[231:243], hybrid_GDP[2:14]-y_GDP[231:243])
res_PCE <- rbind(arx_PCE[1,4:16]-y_PCE[231:243], ann_PCE[2:14]-y_PCE[231:243], hybrid_PCE[2:14]-y_PCE[231:243])

#sourcing sum_res function
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/sum_res.R")

#these matrices give the sum of residuals in each timepoint
sum_res_GDP <- as.matrix(sum_res(res_data = res_GDP))
sum_res_PCE <- as.matrix(sum_res(res_data = res_PCE))

#plotting the sum of residuals
plot(sum_res_GDP[1,], type = "line", col = "black", main = "Sum of residuals (GDP)",
      xlab = "Forecasting horizon", ylab = "Sum of residuals")
graphics::lines(sum_res_GDP[2,], col = "grey")
graphics::lines(sum_res_GDP[3,], col = "blue")
graphics::legend(x = 8.5, y = 1.75, c("ARX-model","ANN-model", "Hybrid model"), col=c('black', 'grey', 'blue'),
       cex=0.55, lty=1:1, lwd=2)

graphics::plot(sum_res_PCE[1,], type = "line", col = "black", main = "Sum of residuals (PCE)",
     xlab = "Forecasting horizon", ylab = "Sum of residuals")
graphics::lines(sum_res_PCE[2,], col = "grey")
graphics::lines(sum_res_PCE[3,], col = "blue")
graphics::legend(x = 8.5, y = 1.75, c("ARX-model","ANN-model", "Hybrid model"), col=c('black', 'grey', 'blue'),
       cex=0.55, lty=1:1, lwd=2)

#DIEBOLD-MARIANO TEST
#comparing the results of the three type of forecasts
dm_p <- data.frame()
dm_p[1,1] <- forecast::dm.test(e1 = forecast_GDP[1,], e2 = forecast_GDP[2,])[[4]]
dm_p[2,1] <- forecast::dm.test(e1 = forecast_GDP[1,], e2 = forecast_GDP[3,])[[4]]
dm_p[3,1] <- forecast::dm.test(e1 = forecast_GDP[1,], e2 = forecast_GDP[4,])[[4]]
dm_p[4,1] <- forecast::dm.test(e1 = forecast_GDP[2,], e2 = forecast_GDP[3,])[[4]]
dm_p[5,1] <- forecast::dm.test(e1 = forecast_GDP[2,], e2 = forecast_GDP[4,])[[4]]
dm_p[6,1] <- forecast::dm.test(e1 = forecast_GDP[3,], e2 = forecast_GDP[4,])[[4]]

dm_p[1,2] <- forecast::dm.test(e1 = forecast_PCE[1,], e2 = forecast_PCE[2,])[[4]]
dm_p[2,2] <- forecast::dm.test(e1 = forecast_PCE[1,], e2 = forecast_PCE[3,])[[4]]
dm_p[3,2] <- forecast::dm.test(e1 = forecast_PCE[1,], e2 = forecast_PCE[4,])[[4]]
dm_p[4,2] <- forecast::dm.test(e1 = forecast_PCE[2,], e2 = forecast_PCE[3,])[[4]]
dm_p[5,2] <- forecast::dm.test(e1 = forecast_PCE[2,], e2 = forecast_PCE[4,])[[4]]
dm_p[6,2] <- forecast::dm.test(e1 = forecast_PCE[3,], e2 = forecast_PCE[4,])[[4]]

#the results
colnames(dm_p) <- c("GDP deflator", "PCE deflator")
format(round(dm_p, 4))

dm_p_alt <- data.frame()
dm_p_alt[1,1] <- forecast::dm.test(e1 = forecast_GDP[1,], e2 = forecast_GDP[2,], alternative = "less")[[4]]
dm_p_alt[2,1] <- forecast::dm.test(e1 = forecast_GDP[1,], e2 = forecast_GDP[3,], alternative = "less")[[4]]
dm_p_alt[3,1] <- forecast::dm.test(e1 = forecast_GDP[1,], e2 = forecast_GDP[4,], alternative = "less")[[4]]
dm_p_alt[4,1] <- forecast::dm.test(e1 = forecast_GDP[2,], e2 = forecast_GDP[3,], alternative = "less")[[4]]
dm_p_alt[5,1] <- forecast::dm.test(e1 = forecast_GDP[2,], e2 = forecast_GDP[4,], alternative = "less")[[4]]
dm_p_alt[6,1] <- forecast::dm.test(e1 = forecast_GDP[3,], e2 = forecast_GDP[4,], alternative = "less")[[4]]

dm_p_alt[1,2] <- forecast::dm.test(e1 = forecast_PCE[1,], e2 = forecast_PCE[2,], alternative = "less")[[4]]
dm_p_alt[2,2] <- forecast::dm.test(e1 = forecast_PCE[1,], e2 = forecast_PCE[3,], alternative = "less")[[4]]
dm_p_alt[3,2] <- forecast::dm.test(e1 = forecast_PCE[1,], e2 = forecast_PCE[4,], alternative = "less")[[4]]
dm_p_alt[4,2] <- forecast::dm.test(e1 = forecast_PCE[2,], e2 = forecast_PCE[3,], alternative = "less")[[4]]
dm_p_alt[5,2] <- forecast::dm.test(e1 = forecast_PCE[2,], e2 = forecast_PCE[4,], alternative = "less")[[4]]
dm_p_alt[6,2] <- forecast::dm.test(e1 = forecast_PCE[3,], e2 = forecast_PCE[4,], alternative = "less")[[4]]

#the results
colnames(dm_p_alt) <- c("GDP deflator", "PCE deflator")
format(round(dm_p_alt, 4))
