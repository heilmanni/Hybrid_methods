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
colnames(x_reg) <- c("RCON", "RINVRESID", "NOUTPUT", "HSTARTS", "EMPLOY", "RUC")

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


arx_GDP <- arx_forecast(lisd = 144, losd = 99, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE <- arx_forecast(lisd = 144, losd = 99, y = y_PCE, xreg = x_reg, max_ar = 5)

#the best model is that where the MSE is the smallest and the p-value of the Ljung-Box test are larger than 0.05
#in both cases this is when phi = 3

gets::arx(y = y_GDP, ar = 1:3, mxreg = x_reg, mc = T)
gets::arx(y = y_PCE, ar = 1:3, mxreg = x_reg, mc = T)


#NEURAL NETWORKS METHODOLOGY
#sourcing ann_forecast function
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/ann_forecast.R")

model_ann <- ann_forecast(ltrain = 100, ltest = 44, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE <- ann_forecast(ltrain = 100, ltest = 44, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann[which.min(model_ann[,3]),]
#now dim = 1, nodes = 4
model_ann_PCE[which.min(model_ann_PCE[,3]),]
#now dim = 1, nodes = 4

#for comparing a forecast and an error value must be given for the out-of-sample data
#using ann_out_forecast function
ann_GDP <- ann_out_forecast(lisd = 144, losd = 99, dim = 1, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.0688
ann_PCE <- ann_out_forecast(lisd = 144, losd = 99, dim = 1, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.2426


#HYBRID METHODOLOGY
#sourcing hybrid_forecast
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/hybrid_forecast.R")

hybrid_GDP <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 144, losd = 99, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)
#MSE = 0.0765
hybrid_PCE <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 144, losd = 99, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)
#MSE = 0.3312


#ANALYSIS
#these matrices give the predicted values of each methods
forecast_GDP <- as.matrix(rbind(arx_GDP[3,4:102], ann_GDP[2:100], hybrid_GDP[2:100], y_GDP[145:243]))
row.names(forecast_GDP) <- c("ARX", "ANN", "Hybrid", "Real")
forecast_PCE <- as.matrix(rbind(arx_PCE[3,4:102], ann_PCE[2:100], hybrid_PCE[2:100], y_PCE[145:243]))
row.names(forecast_PCE) <- c("ARX", "ANN", "Hybrid", "Real")

#these matrices give the residuals of each methods
res_GDP <- rbind(arx_GDP[3,4:102]-y_GDP[145:243], ann_GDP[2:100]-y_GDP[145:243], hybrid_GDP[2:100]-y_GDP[145:243])
res_PCE <- rbind(arx_PCE[3,4:102]-y_PCE[145:243], ann_PCE[2:100]-y_PCE[145:243], hybrid_PCE[2:100]-y_PCE[145:243])

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

#sourcing dm_test
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/dm_test.R")

dm_p <- dm_test(imput_matrix_1 = forecast_GDP, imput_matrix_2 = forecast_PCE, alternative = "two.sided")
dm_p_alter <- dm_test(imput_matrix_1 = forecast_GDP_20, imput_matrix_2 = forecast_PCE_20, alternative = "less")



#ROBUSTNESS
#LOSD denotes the length of out-of-sample data

#1. LOSD = 49 (20%)
#1.A. ARX-model
arx_GDP_20 <- arx_forecast(lisd = 194, losd = 49, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE_20 <- arx_forecast(lisd = 194, losd = 49, y = y_PCE, xreg = x_reg, max_ar = 5)

#1.B. ANN-model
model_ann_GDP_20 <- ann_forecast(ltrain = 136, ltest = 58, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_20 <- ann_forecast(ltrain = 136, ltest = 58, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

model_ann_GDP_20[which.min(model_ann_GDP_20[,3]),]
#now dim = 2, nodes = 4
model_ann_PCE_20[which.min(model_ann_PCE_20[,3]),]
#now dim = 1, nodes = 4

ann_GDP_20 <- ann_out_forecast(lisd = 194, losd = 49, dim = 2, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.1046
ann_PCE_20 <- ann_out_forecast(lisd = 194, losd = 49, dim = 1, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.3826


#1.C. Hybrid-model
hybrid_GDP_20 <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 194, losd = 49, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)
hybrid_PCE_20 <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 194, losd = 49, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)


#2. LOSD = 73 (30%)
#2.A. ARX-model
arx_GDP_30 <- arx_forecast(lisd = 170, losd = 73, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE_30 <- arx_forecast(lisd = 170, losd = 73, y = y_PCE, xreg = x_reg, max_ar = 5)

#2.B. ANN-model
model_ann_GDP_30 <- ann_forecast(ltrain = 119, ltest = 51, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_30 <- ann_forecast(ltrain = 119, ltest = 51, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

model_ann_GDP_30[which.min(model_ann_GDP_30[,3]),]
#now dim = 1, nodes = 3
model_ann_PCE_30[which.min(model_ann_PCE_30[,3]),]
#now dim = 1, nodes = 4

ann_GDP_30 <- ann_out_forecast(lisd = 170, losd = 73, dim = 1, nodes = 3, y = y_GDP, xreg = x_reg)
#MSE = 0.0911
ann_PCE_30 <- ann_out_forecast(lisd = 170, losd = 73, dim = 1, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.3103


#2.C. Hybrid-model
hybrid_GDP_30 <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 170, losd = 73, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)
hybrid_PCE_30 <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 170, losd = 73, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)



#3. LOSD = 122 (50%)
#3.A. ARX-model
arx_GDP_50 <- arx_forecast(lisd = 121, losd = 122, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE_50 <- arx_forecast(lisd = 121, losd = 122, y = y_PCE, xreg = x_reg, max_ar = 5)

#3.B. ANN-model
model_ann_GDP_50 <- ann_forecast(ltrain = 85, ltest = 36, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_50 <- ann_forecast(ltrain = 85, ltest = 36, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

model_ann_GDP_50[which.min(model_ann_GDP_50[,3]),]
#now dim = 4, nodes = 4
model_ann_PCE_50[which.min(model_ann_PCE_50[,3]),]
#now dim = 4, nodes = 4

ann_GDP_50 <- ann_out_forecast(lisd = 121, losd = 122, dim = 4, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.0572
ann_PCE_50 <- ann_out_forecast(lisd = 121, losd = 122, dim = 4, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.1829

#3.C. Hybrid-model
hybrid_GDP_50 <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 121, losd = 122, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)
hybrid_PCE_50 <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 121, losd = 122, phi = 1, brake = 0.7, max_dim = 2, max_nodes = 2)


#ANALYSIS
#these matrices give the predicted values of each methods

#LOSD = 20
forecast_GDP_20 <- as.matrix(rbind(arx_GDP_20[1,5:53], ann_GDP_20[2:50], hybrid_GDP_20[2:50], y_GDP[195:243]))
row.names(forecast_GDP_20) <- c("ARX", "ANN", "Hybrid", "Real")
forecast_PCE_20 <- as.matrix(rbind(arx_PCE_20[1,5:53], ann_PCE_20[2:50], hybrid_PCE_20[2:50], y_PCE[195:243]))
row.names(forecast_PCE_20) <- c("ARX", "ANN", "Hybrid", "Real")

#LOSD = 30
forecast_GDP_30 <- as.matrix(rbind(arx_GDP_30[2,5:77], ann_GDP_30[2:74], hybrid_GDP_30[2:74], y_GDP[171:243]))
row.names(forecast_GDP_30) <- c("ARX", "ANN", "Hybrid", "Real")
forecast_PCE_30 <- as.matrix(rbind(arx_PCE_30[1,5:77], ann_PCE_30[2:74], hybrid_PCE_30[2:74], y_PCE[171:243]))
row.names(forecast_PCE_30) <- c("ARX", "ANN", "Hybrid", "Real")

#LOSD = 50
forecast_GDP_50 <- as.matrix(rbind(arx_GDP_50[5,5:126], ann_GDP_50[2:123], hybrid_GDP_50[2:123], y_GDP[122:243]))
row.names(forecast_GDP_50) <- c("ARX", "ANN", "Hybrid", "Real")
forecast_PCE_50 <- as.matrix(rbind(arx_PCE_50[3,5:126], ann_PCE_50[2:123], hybrid_PCE_50[2:123], y_PCE[122:243]))
row.names(forecast_PCE_50) <- c("ARX", "ANN", "Hybrid", "Real")


#DIEBOLD-MARIANO TESTS
dm_test_20 <- dm_test(imput_matrix_1 = forecast_GDP_20, imput_matrix_2 = forecast_PCE_20, alternative = "two.sided")
dm_test_20_alter <- dm_test(imput_matrix_1 = forecast_GDP_20, imput_matrix_2 = forecast_PCE_20, alternative = "less")

dm_test_30 <- dm_test(imput_matrix_1 = forecast_GDP_30, imput_matrix_2 = forecast_PCE_30, alternative = "two.sided")
dm_test_30_alter <- dm_test(imput_matrix_1 = forecast_GDP_30, imput_matrix_2 = forecast_PCE_30, alternative = "less")

dm_test_50 <- dm_test(imput_matrix_1 = forecast_GDP_50, imput_matrix_2 = forecast_PCE_50, alternative = "two.sided")
dm_test_50_alter <- dm_test(imput_matrix_1 = forecast_GDP_50, imput_matrix_2 = forecast_PCE_50, alternative = "less")
