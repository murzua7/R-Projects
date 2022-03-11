#EH Clio Lab Data

attach(PIBxGastoFiscal)
names(PIBxGastoFiscal)
summary(PIBxGastoFiscal)
plot(GastoFiscal ~ PIBReal)
plot(PIBReal ~ GastoFiscal)
abline(plot(PIBReal ~ GastoFiscal))

tsPIB <- ts(data = PIBxGastoFiscal, start = 1833, end = 2009, frequency = 1)
plot(tsPIB)

modelPIB <- lm(PIBReal ~ GastoFiscal)
plot(modelPIB)
Enter
Summary(modelPIB)
coef(modelPIB)
plot(summary(modelPIB))
plot(PIBxGastoFiscal, headings = FALSE, method = "render")


sjPlot::tab_model(list(modelPIB), show.ci=FALSE, p.style = "stars",string.pred = "Predictores", string.est = "??",digits = 3,
                  dv.labels = c("Modelo 1"))
stargazer(modelPIB, type = "html", out = "Trabajos en R/Trabajos en R")


gam(PIBReal ~ s(GastoFiscal), method = "REML")
gamf <- gam(PIBReal ~ s(GastoFiscal), method = "REML")

plot(gamf)

#Banco central

head(PIB_Trimestral)
nrow(PIB_Trimestral)
names(PIB_Trimestral)
names(PIB_Trimestral)
attach(PIB_Trimestral)


tsPIB <- ts(PIB_Trimestral$PIB, frequency = 4, start = 1996)
autoplot(tsPIB)

summary(tsPIB)

ggAcf(tsPIB) + ggtitle("ACF de PIB de Chile trimestral encadenado") 
ggPacf(tsPIB)

difftspib <- diff(tsPIB)

ggAcf(difftspib) + ggtitle("ACF de PIB de Chile trimestral encadenado") 
ggPacf(difftspib)

ts_decompose(tsPIB, type = "additive", showline = TRUE)
 
adf.test(tsPIB, k = 3)
pp.test(tsPIB)
kpss.test(tsPIB)
#es no estacionaria excepto para pptest, tambien lo indica la literatura https://www.jstor.org/stable/1061713?seq=1

#In sample forecasting

#Split the sample

split_gdp <- ts_split(tsPIB, sample.out = 5)

training <- split_gdp$train
testing <- split_gdp$test

length(training)
length(testing)

#Use ARIMA and SARIMA

arima_diag(training)
arima211 <- arima(training, order = c(3,1,0))
arima211
autoplot(arima211)
check_res(arima211)

sarima2111 <- arima(training, order = c(3,1,0), seasonal = list(order = c(0,1,1)), method = "ML")
autoplot(sarima2111)
check_res(sarima2111)

auto <- auto.arima(training, seasonal = TRUE)
auto
autoplot(auto)
check_res(auto)

#forecast values

fcast1 <- forecast(arima211, h = 5)
test_forecast(actual = tsPIB, forecast.obj = fcast1, test = testing)
accuracy(fcast1, testing)

fcast2 <- forecast(sarima2111, h = 5)
test_forecast(actual = tsPIB, forecast.obj = fcast2, test = testing)
accuracy(fcast2, testing)

fcasta <- forecast(auto, h = 5)
test_forecast(actual = tsPIB, forecast.obj = fcasta, test = testing)
accuracy(fcasta, testing)

#forecast GAM


transform(Periodo)
dperiodo <- as.Date(PIB_Trimestral$Periodo)
nperiodo <- as.numeric(dperiodo)
asperiodo <- as.Date(nperiodo, "1970-01-01")
nperiodo

gamPIB <- gam(PIB ~ s(nperiodo), method = "REML", data = PIB_Trimestral)
plot(gamPIB)

k <- c(18535, 18626, 18717)
predict.gam(gamPIB, newdata = "k")
predgam <- predict(gamPIB)

#Base de datos experimental

attach(Base_de_datos_experimental)
names(Base_de_datos_experimental)
gamexp <- gam(PIBReal ~ s(ElectPC) + s(TCReal), method = "REML")
gamexp

#Crear ts

tspib <- ts(Base_de_datos_experimental$PIBReal, start = 1927, frequency = 1)
tselectpc <- ts(Base_de_datos_experimental$ElectPC, start = 1927, frequency = 1)
tstcreal <- ts(Base_de_datos_experimental$TCReal, start = 1927, frequency = 1)
tssalarior <- ts(Base_de_datos_experimental$SalarioR, start = 1927, frequency = 1)
tstinteres <- ts(Base_de_datos_experimental$TInteresR, start = 1927, frequency = 1)
tsdeux <- ts(Base_de_datos_experimental$DeuExPu, start = 1927, frequency = 1)

#Chequear diff

ndiffs(tspib)
ndiffs(tselectpc)
ndiffs(tstcreal)
ndiffs(tssalarior)
ndiffs(tstinteres)
ndiffs(tsdeux)

#Diferenciar variables

dtspib1 <- diff(tspib)
dtspib2 <- diff(dtspib1)

dtselect1 <- diff(tselectpc)
dtselect2 <- diff(dtselect1)

dtstcreal1 <- diff(tstcreal)
dtstcreal2 <- diff(dtstcreal1)

dtss1 <- diff(tssalarior)
dtss2 <- diff(dtss1)

dtst1 <- diff(tstinteres)
dtst2 <- diff(dtst1)

dtsdeux1 <- diff(tsdeux)
dtsdeux2 <- diff(dtsdeux1)

#Hacer test causalidad de granger

#Crear ts con variables diferenciadas

dvar1 <- ts(dtspib2, start = 1927, frequency = 1)
dvar2 <- ts(dtselect2, start = 1927, frequency = 1)
dvar3 <- ts(dtstcreal2, start = 1927, frequency = 1)
dvar4 <- ts(dtss2, start = 1927, frequency = 1)
dvar5 <- ts(dtst2, start = 1927, frequency = 1)
dvar6 <- ts(dtsdeux2, start = 1927, frequency = 1)

tsfinal <- cbind(dvar1, dvar4, dvar6)
print(tsfinal)

#Crear VAR

VARselect(tsfinal)
VAR1 <- VAR(tsfinal, p=9)
VAR1
summary(VAR1)
roots(VAR1)
plot(stability(VAR1))
