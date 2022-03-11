#Para comenzar

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


my_packages <- c("dplyr", "tidyverse", "RMySQL", "data.table", "mgcv", "DataEditR", 'corrplot')

ipak(my_packages)

options(scipen = 999)

my_packages2 <- c("sjPlot", "summarytools", "fastDummies", "sjlabelled", "coefplot")

ipak(my_packages2)

#nonlinear autoregressive (AR) time series models
library(tsDyn)
# Interactive Web Graphics
library(plotly)
#HTML Widgets for R
library(htmlwidgets)
#Take Screenshots of Web Pages
library(webshot)
#Summary Statistics Tables
library(stargazer)
#Time Series Analysis and Computational Finance
library(tseries)
#Tidyverse packages
library(tidyverse)
#Forecasting Functions for Time Series and Linear Models
library(forecast)
#Unit Root and Cointegration Tests for Time Series Data
library(urca)
#Functions for Time Series Analysis and Forecasting
library(TSstudio)
#Vector Auto Regression Modelling
library(vars)
#Alternative Time Series Analysis
library(aTSA)
#GAMs
library(ecm) 
#Insertar linea en graficos
library(basicTrendline)
#Lotka Volterra
library(primer)
#Funciones matematicas
library(pracma)
#Partial Least Squares
library(pls)
#Generalized Linear Models
library(glmnet)
#Classification and Regression Training
library(caret)
#Evaluation Metrics for Regression and TS
library(Metrics)
#Test de LM
library(lmtest)
#Para descriptivos
library(psych)
#Para generar tablas
library(formattable)
#SimulacionMonteCarlo 
library(MonteCarlo)
#Bayesian Impact Analysis
library(CausalImpact)
#Interrupted time series
library(its.analysis)
#Para usar las dos anteriores usar tibble
library(tibble)
#Bayesian structural time series model
library(bsts)
#change point analysis
library(changepoint)
# dynamic time warping
library(dtw)
#causal impact made easy
library(MarketMatching)
#synthetic controls
library(Synth)
#zoo
library(zoo)
#test models
library(lme4)
#efectos margianles
library(margins)
#Imputation
library(mice)
#ggplot2 UI
library(esquisse)
#parallel processing
library(foreach)
#doParallel
library(doParallel)
#LaTeX
library(tinytex)
#Explanatory Analysis
library(modelStudio)
#Reshape
library(reshape)
#bayesian linear regression
library(rstanarm)
