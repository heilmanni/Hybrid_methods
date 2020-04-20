#By using this file we can forecast in four different ways: with an ARX, an ANN, an averaging and a hybrid methology

#the source of the data are from the  Federal Reserve of Philadelphia: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/data-files
#the exact source of each variable are the followings:
#P GNP/GDP price index: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/data-files/p
#PCON Price index for personal consumption expenditures, constructed: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/data-files/pcon
#RCON Real personal consumption expenditure: total: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/data-files/rcon
#RINVRESID Real gross private domestic investment: residential: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/datafiles/rinvresid
#NOUTPUT Nominal GNP/GDP: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/datafiles/noutput
#HSTARTS Housing starts: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/datafiles/hstarts
#EMPLOY Nonfarm payroll employment: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/datafiles/employ
#RUC Unemployment rate: https://www.philadelphiafed.org/research-anddata/real-time-center/real-time-data/data-files/ruc

#necessary packages
require(readxl)
require(forecast)
require(ModelMetrics)
require(gets)
require(readxl)
require(stats)
require(graphics)

#in ordet to have always the same results:
set.seed(2365)

#IMPORT TIMESERIES
#importing Phil_data.xlsx file
phil_data <- readxl::read_xlsx("C:/Users/Istvan/Documents/EFRP_D/Phil_data.xlsx", col_names = T)

#the output variable is the inflation; there is two options, based on GDP deflator and PCE deflator
y_GDP <- phil_data[,2]
y_GDP <- y_GDP[[1]]
y_PCE <- phil_data[,3]
y_PCE <- y_PCE[[1]]

y_results <- cbind(y_GDP, y_PCE)

#the regressors are the columns from 3 up to 9
x_reg <- phil_data[,4:9]
x_reg <- cbind(x_reg[[1]], x_reg[[2]], x_reg[[3]], x_reg[[4]], x_reg[[5]], x_reg[[6]])
colnames(x_reg) <- c("RCON", "RINVRESID", "NOUTPUT", "HSTARTS", "EMPLOY", "RUC")

#PLOTTING, STATISTICS
#plot of the results variables
plot(y_GDP, type = "l")
lines(y_PCE, col = "blue")

#plot of the regressors
plot(x_reg[,6], type = "l")

#some statistical data
#sorucing the statistics function
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/statistics.R")

statistics(input = x_reg)
statistics(input = y_results)

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

model_ann_GDP <- ann_forecast(ltrain = 100, ltest = 44, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE <- ann_forecast(ltrain = 100, ltest = 44, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann_GDP[which.min(model_ann_GDP[,3]),]
#now dim = 2, nodes = 4, MSE = 0.02778163
model_ann_PCE[which.min(model_ann_PCE[,3]),]
#now dim = 1, nodes = 4, MSE = 0.04713021

#for comparing a forecast and an error value must be given for the out-of-sample data
#using ann_out_forecast function
ann_GDP <- ann_out_forecast(lisd = 144, losd = 99, dim = 2, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.07128219
ann_PCE <- ann_out_forecast(lisd = 144, losd = 99, dim = 1, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.2392148


#AVERAGING
average_GDP <- as.numeric((ann_GDP[2:100] + arx_GDP[3,5:103])/2)
ModelMetrics::mse(actual = y_GDP[145:243], predicted = average_GDP)
#MSE = 0.07121739
average_PCE <- as.numeric((ann_PCE[2:100] + arx_PCE[3,5:103])/2)
ModelMetrics::mse(actual = y_PCE[145:243], predicted = average_PCE)
#MSE = 0.2658606


#HYBRID METHODOLOGY
#sourcing hybrid_forecast
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/hybrid_forecast.R")

hybrid_GDP <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 144, losd = 99, phi = 3, brake = 0.7, max_dim = 4, max_nodes = 4)
#MSE = 0.06969837, dim = 4, nodes = 4
hybrid_PCE <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 144, losd = 99, phi = 3, brake = 0.7, max_dim = 4, max_nodes = 4)
#MSE = 0.2199841, dim = 3, nodes = 4


#ANALYSIS
#these matrices give the predicted values of each methods
forecast_GDP <- as.matrix(rbind(arx_GDP[3,4:102], ann_GDP[2:100], average_GDP, as.numeric(hybrid_GDP[4:102]), y_GDP[145:243]))
row.names(forecast_GDP) <- c("ARX", "ANN", "Averaging", "Hybrid", "Real")
forecast_PCE <- as.matrix(rbind(arx_PCE[3,4:102], ann_PCE[2:100], average_PCE, as.numeric(hybrid_PCE[4:102]), y_PCE[145:243]))
row.names(forecast_PCE) <- c("ARX", "ANN", "Averaging", "Hybrid", "Real")

#these matrices give the residuals of each methods
res_GDP <- rbind(as.numeric(y_GDP[145:243] - arx_GDP[3,5:103]), as.numeric(y_GDP[145:243] - ann_GDP[2:100]), as.numeric(y_GDP[145:243] - average_GDP), as.numeric(y_GDP[145:243] - hybrid_GDP[4:102]))
row.names(res_GDP) <- c("ARX", "ANN", "Averaging", "Hybrid")

res_PCE <- rbind(as.numeric(y_PCE[145:243] - arx_PCE[3,5:103]), as.numeric(y_PCE[145:243] - ann_PCE[2:100]), as.numeric(y_PCE[145:243] - average_PCE), as.numeric(y_PCE[145:243] - hybrid_PCE[4:102]))
row.names(res_PCE) <- c("ARX", "ANN", "Averaging", "Hybrid")

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
graphics::lines(sum_res_GDP[4,], col = "red")

graphics::plot(sum_res_PCE[1,], type = "line", col = "black", main = "Sum of residuals (PCE)",
     xlab = "Forecasting horizon", ylab = "Sum of residuals")
graphics::lines(sum_res_PCE[2,], col = "grey")
graphics::lines(sum_res_PCE[3,], col = "blue")
graphics::lines(sum_res_GDP[4,], col = "red")


#DIEBOLD-MARIANO TEST
#comparing the results of the three type of forecasts

#sourcing dm_test
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/dm_test.R")

dm_p <- dm_test(imput_matrix_1 = res_GDP, imput_matrix_2 = res_PCE, alternative = "two.sided")
dm_p_alter <- dm_test(imput_matrix_1 = res_GDP, imput_matrix_2 = res_PCE, alternative = "less")
dm_p_alter_2 <- dm_test(imput_matrix_1 = res_GDP, imput_matrix_2 = res_PCE, alternative = "greater")


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
#MSE = 0.09795146
ann_PCE_20 <- ann_out_forecast(lisd = 194, losd = 49, dim = 1, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.363533

#1.C. Averaging
average_GDP_20 <- as.numeric((arx_GDP_20[1,5:53] + ann_GDP_20[2:50])/2)
ModelMetrics::mse(actual = y_GDP[195:243], predicted = average_GDP_20)
#MSE = 0.1028038
average_PCE_20 <- as.numeric((arx_PCE_20[1,5:53] + ann_PCE_20[2:50])/2)
ModelMetrics::mse(actual = y_PCE[195:243], predicted = average_PCE_20)
#MSE = 0.43973

#1.D. Hybrid-model
hybrid_GDP_20 <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 194, losd = 49, phi = 1, brake = 0.7, max_dim = 4, max_nodes = 4)
hybrid_PCE_20 <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 194, losd = 49, phi = 1, brake = 0.7, max_dim = 4, max_nodes = 4)


#2. LOSD = 73 (30%)
#2.A. ARX-model
arx_GDP_30 <- arx_forecast(lisd = 170, losd = 73, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE_30 <- arx_forecast(lisd = 170, losd = 73, y = y_PCE, xreg = x_reg, max_ar = 5)

#2.B. ANN-model
model_ann_GDP_30 <- ann_forecast(ltrain = 119, ltest = 51, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_30 <- ann_forecast(ltrain = 119, ltest = 51, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

model_ann_GDP_30[which.min(model_ann_GDP_30[,3]),]
#now dim = 2 nodes = 4
model_ann_PCE_30[which.min(model_ann_PCE_30[,3]),]
#now dim = 1, nodes = 3

ann_GDP_30 <- ann_out_forecast(lisd = 170, losd = 73, dim = 2, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.0911
ann_PCE_30 <- ann_out_forecast(lisd = 170, losd = 73, dim = 1, nodes = 3, y = y_PCE, xreg = x_reg)
#MSE = 0.3103


#2.C. Averaging
average_GDP_30 <- as.numeric((arx_GDP_30[2,5:77] + ann_GDP_30[2:74])/2)
ModelMetrics::mse(actual = y_GDP[171:243], predicted = average_GDP_30)
#MSE = 0.09277077
average_PCE_30 <- as.numeric((arx_PCE_30[1,5:77] + ann_PCE_30[2:74])/2)
ModelMetrics::mse(actual = y_PCE[171:243], predicted = average_PCE_30)
#MSE = 0.3730829


#2.D. Hybrid-model
hybrid_GDP_30 <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 170, losd = 73, phi = 2, brake = 0.7, max_dim = 4, max_nodes = 4)
hybrid_PCE_30 <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 170, losd = 73, phi = 1, brake = 0.7, max_dim = 4, max_nodes = 4)



#3. LOSD = 122 (50%)
#3.A. ARX-model
arx_GDP_50 <- arx_forecast(lisd = 121, losd = 122, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_PCE_50 <- arx_forecast(lisd = 121, losd = 122, y = y_PCE, xreg = x_reg, max_ar = 5)

#3.B. ANN-model
model_ann_GDP_50 <- ann_forecast(ltrain = 85, ltest = 36, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_50 <- ann_forecast(ltrain = 85, ltest = 36, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

model_ann_GDP_50[which.min(model_ann_GDP_50[,3]),]
#now dim = 2, nodes = 4
model_ann_PCE_50[which.min(model_ann_PCE_50[,3]),]
#now dim = 4, nodes = 4

ann_GDP_50 <- ann_out_forecast(lisd = 121, losd = 122, dim = 2, nodes = 4, y = y_GDP, xreg = x_reg)
#MSE = 0.05838324
ann_PCE_50 <- ann_out_forecast(lisd = 121, losd = 122, dim = 4, nodes = 4, y = y_PCE, xreg = x_reg)
#MSE = 0.173395

#3.C. Averaging
average_GDP_50 <- as.numeric((arx_GDP_50[5,5:126] + ann_GDP_50[2:123])/2)
ModelMetrics::mse(actual = y_GDP[122:243], predicted = average_GDP_50)
#MSE = 0.05711613
average_PCE_50 <- as.numeric((arx_PCE_50[3,5:126] + ann_PCE_50[2:123])/2)
ModelMetrics::mse(actual = y_PCE[122:243], predicted = average_PCE_50)
#MSE = 0.1891291

#3.D. Hybrid-model
hybrid_GDP_50 <- hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 121, losd = 122, phi = 5, brake = 0.7, max_dim = 4, max_nodes = 4)
hybrid_PCE_50 <- hybrid_forecast(y = y_PCE, xreg = x_reg, lisd = 121, losd = 122, phi = 3, brake = 0.7, max_dim = 4, max_nodes = 4)


#ANALYSIS
#these matrices give the predicted values of each methods

#LOSD = 20
forecast_GDP_20 <- as.matrix(rbind(arx_GDP_20[1,5:53], ann_GDP_20[2:50], average_GDP_20, hybrid_GDP_20[4:52], y_GDP[195:243]))
row.names(forecast_GDP_20) <- c("ARX", "ANN", "Average", "Hybrid", "Real")
res_GDP_20 <- rbind(as.numeric(y_GDP[195:243] - arx_GDP_20[1,5:49]), as.numeric(y_GDP[195:243] - ann_GDP_20[2:50]),
                    as.numeric(y_GDP[195:243] - average_GDP_20), as.numeric(y_GDP[195:243] - hybrid_GDP_20[4:52]))
row.names(res_GDP_20) <- c("ARX", "ANN", "Average", "Hybrid")

forecast_PCE_20 <- as.matrix(rbind(arx_PCE_20[1,5:53], ann_PCE_20[2:50], average_PCE_20, hybrid_PCE_20[4:52], y_PCE[195:243]))
row.names(forecast_PCE_20) <- c("ARX", "ANN", "Average", "Hybrid", "Real")
res_PCE_20 <- rbind(as.numeric(y_PCE[195:243] - arx_PCE_20[1,5:49]), as.numeric(y_PCE[195:243] - ann_PCE_20[2:50]),
                    as.numeric(y_PCE[195:243] - average_PCE_20), as.numeric(y_PCE[195:243] - hybrid_PCE_20[4:52]))
row.names(res_PCE_20) <- c("ARX", "ANN", "Average", "Hybrid")


#LOSD = 30
forecast_GDP_30 <- as.matrix(rbind(arx_GDP_30[2,5:77], ann_GDP_30[2:74], average_GDP_30, hybrid_GDP_30[4:76], y_GDP[171:243]))
row.names(forecast_GDP_30) <- c("ARX", "ANN", "Average", "Hybrid", "Real")
res_GDP_30 <- rbind(as.numeric(y_GDP[171:243] - arx_GDP_30[2,5:77]), as.numeric(y_GDP[171:243] - ann_GDP_30[2:74]),
                    as.numeric(y_GDP[171:243] - average_GDP_30), as.numeric(y_GDP[171:243] - hybrid_GDP_30[4:76]))
row.names(res_GDP_30) <- c("ARX", "ANN", "Average", "Hybrid")

#plot of the residuals
plot(res_GDP["ARX",], type = "l")
lines(res_GDP["ANN",], col = "red")
lines(res_GDP["Hybrid",], col = "green")

forecast_PCE_30 <- as.matrix(rbind(arx_PCE_30[1,5:77], ann_PCE_30[2:74], average_PCE_30, hybrid_PCE_30[4:76], y_PCE[171:243]))
row.names(forecast_PCE_30) <- c("ARX", "ANN", "Average", "Hybrid", "Real")
res_PCE_30 <- rbind(as.numeric(y_PCE[171:243] - arx_PCE_30[2,5:77]), as.numeric(y_PCE[171:243] - ann_PCE_30[2:74]),
                    as.numeric(y_PCE[171:243] - average_PCE_30), as.numeric(y_PCE[171:243] - hybrid_PCE_30[4:76]))
row.names(res_PCE_30) <- c("ARX", "ANN", "Average", "Hybrid")


#LOSD = 50
forecast_GDP_50 <- as.matrix(rbind(arx_GDP_50[5,5:126], ann_GDP_50[2:123], average_GDP_50, hybrid_GDP_50[4:125], y_GDP[122:243]))
row.names(forecast_GDP_50) <- c("ARX", "ANN", "Average", "Hybrid", "Real")
res_GDP_50 <- rbind(as.numeric(y_GDP[122:243] - arx_GDP_50[5,5:126]), as.numeric(y_GDP[122:243] - ann_GDP_50[2:123]),
                    as.numeric(y_GDP[122:243] - average_GDP_50), as.numeric(y_GDP[122:243] - hybrid_GDP_50[4:125]))
row.names(res_GDP_50) <- c("ARX", "ANN", "Average", "Hybrid")

#plot of the residuals
plot(res_GDP_50["ARX",], type = "l")
lines(res_GDP_50["ANN",], col = "green")
lines(res_GDP_50["Average",], col = "blue")
lines(res_GDP_50["Hybrid",], col = "red")

forecast_PCE_50 <- as.matrix(rbind(arx_PCE_50[3,5:126], ann_PCE_50[2:123], average_PCE_50, hybrid_PCE_50[4:125], y_PCE[122:243]))
row.names(forecast_PCE_50) <- c("ARX", "ANN", "Average", "Hybrid", "Real")
res_PCE_50 <- rbind(as.numeric(y_PCE[122:243] - arx_PCE_50[5,5:126]), as.numeric(y_PCE[122:243] - ann_PCE_50[2:123]),
                    as.numeric(y_PCE[122:243] - average_PCE_50), as.numeric(y_PCE[122:243] - hybrid_PCE_50[4:125]))
row.names(res_PCE_50) <- c("ARX", "ANN", "Average", "Hybrid")

#plot of the residuals
plot(res_PCE_50["ARX",], type = "l")
lines(res_PCE_50["ANN",], col = "green")
lines(res_PCE_50["Average",], col = "blue")
lines(res_PCE_50["Hybrid",], col = "red")


#DIEBOLD-MARIANO TESTS
dm_test_20 <- dm_test(imput_matrix_1 = res_GDP_20, imput_matrix_2 = res_PCE_20, alternative = "two.sided")
dm_test_20_alter <- dm_test(imput_matrix_1 = res_GDP_20, imput_matrix_2 = res_PCE_20, alternative = "less")
dm_test_20_alter_2 <- dm_test(imput_matrix_1 = res_GDP_20, imput_matrix_2 = res_PCE_20, alternative = "greater")

dm_test_30 <- dm_test(imput_matrix_1 = res_GDP_30, imput_matrix_2 = res_PCE_30, alternative = "two.sided")
dm_test_30_alter <- dm_test(imput_matrix_1 = res_GDP_30, imput_matrix_2 = res_PCE_30, alternative = "less")
dm_test_30_alter_2 <- dm_test(imput_matrix_1 = res_GDP_30, imput_matrix_2 = res_PCE_30, alternative = "greater")

dm_test_50 <- dm_test(imput_matrix_1 = res_GDP_50, imput_matrix_2 = res_PCE_50, alternative = "two.sided")
dm_test_50_alter <- dm_test(imput_matrix_1 = res_GDP_50, imput_matrix_2 = res_PCE_50, alternative = "less")
dm_test_50_alter_2 <- dm_test(imput_matrix_1 = res_GDP_50, imput_matrix_2 = res_PCE_50, alternative = "greater")


#ROBUSTNESS 2: TRAIN AND TEST SETS

#1. TRAINING SET IS 72 ELEMENTS LONG (50%)
model_ann_GDP_T_50 <- ann_forecast(ltrain = 72, ltest = 72, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_T_50 <- ann_forecast(ltrain = 72, ltest = 72, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann_GDP_T_50[which.min(model_ann_GDP_T_50[,3]),]
#now dim = 4, nodes = 4, MSE = 0.02232758
model_ann_PCE_T_50[which.min(model_ann_PCE_T_50[,3]),]
#now dim = 4, nodes = 4, MSE = 0.03694844


#2. TRAINING SET IS 86 ELEMENTS LONG (60%)
model_ann_GDP_T_60 <- ann_forecast(ltrain = 86, ltest = 50, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_T_60 <- ann_forecast(ltrain = 86, ltest = 50, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann_GDP_T_60[which.min(model_ann_GDP_T_60[,3]),]
#now dim = 1, nodes = 4, MSE = 0.02517302
model_ann_PCE_T_60[which.min(model_ann_PCE_T_60[,3]),]
#now dim = 1, nodes = 4, MSE = 0.04423072


#3. TRAINING SET IS 122 ELEMENTS LONG (85%)
model_ann_GDP_T_85 <- ann_forecast(ltrain = 122, ltest = 22, y = y_GDP, xreg = x_reg, max_dim = 4, max_nodes = 4)
model_ann_PCE_T_85 <- ann_forecast(ltrain = 122, ltest = 22, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann_GDP_T_85[which.min(model_ann_GDP_T_85[,3]),]
#now dim = 1, nodes = 3, MSE = 0.02692582
model_ann_PCE_T_85[which.min(model_ann_PCE_T_85[,3]),]
#now dim = 1, nodes = 3, MSE = 0.0427274
