#Question 1

library(readxl)
library(tidyverse)
library(ISLR) 
library(ggplot2) 
library(reshape2) 
library(plyr) 
library(dplyr) 
library(class)
library(haven)
library(glmnet)

d = read_xlsx("JSTdatasetR3.xlsx", sheet = 2)

d <- JSTdatasetR3

#1

d <- filter(d, country == "Canada")
attach(d)
d <- data.frame(rgdpmad,rgdppc,cpi,stir,ltrate,crisisJST,year)

#2

summary(d)
d %>% map(~ggplot(d, aes(x = year, y = .)) + geom_line())

#3

library(foreach)
library(class)

d1 <- data.frame(rgdpmad,rgdppc,cpi,stir,ltrate,crisisJST) %>% na.omit()

d <- filter(d, country == "Canada")
f <- filter(f, country == "Canada")

f1 = d[d$year >= 1995,]
f2 = d[,colnames(d) %in% c('rgdpmad','rgdppc','cpi',"stir","ltrate",'crisisJST')]
summary(f2)
plot(f2)
f2 = f2[complete.cases(f2),]
dim(f2)

##fixed k

y = f2$crisisJST
x = cbind(f2$rgdpmad,f2$rgdpmad,f2$cpi,f2$stir,f2$ltrate)
x = scale(x)
k=3

train.knn.pred = knn(x,x,y,k=k)
mean(y != train.knn.pred)

# Train - test split
N = dim(f2)

kseq = (1:10)
th = 0.8

N = dim(f2)
Ntrain = round(N[1] * th,0)

s1 = sample(N[1],Ntrain,replace = FALSE)

Train = f2[s1,]
Test = f2[-s1,]

pred_error = rep(0,10)

xtrain = Train[,colnames(Train) != "crisisJST"]
xtest = Test[,colnames(Test) != 'crisisJST']

ytrain = Train$crisisJST
ytest = Test$crisisJST

train.knn.pred = knn(xtrain,xtest,ytrain,k=4)
train.knn.pred

mean(ytest!=train.knn.pred)

#foreach 

foreach(i=kseq)%dopar%{
  xtrain = Train[,colnames(Train) != 'crisisJST']
  xtest = Test[,colnames(Test) != 'crisisJST']
  
  ytrain = Train$crisisJST
  ytest = Test$crisisJST
  
  train.knn.pred = knn(xtrain,xtest,ytrain,k=i)
  pred_error[i] = mean(ytest != train.knn.pred)
}

plot(kseq,pred_error,type='h')

#4


logitmodel <- glm(crisisJST ~ rgdpmad + rgdppc + cpi + stir + ltrate, data = f2, family = binomial(link = "probit"))
pred = predict(logitmodel,xtest,type = 'response')
predclass = ifelse(pred > 0.5,1,0)

#Question 2

#1

library(haven)

df <- read_dta("PriceData.dta")
summary(df)

#2 

attach(df)
data.frame(metalc,fuelc,crudeoilc,naturalgasc,coalc,fuell,crudeoill,
           naturalgasl,coall) %>%
  cor() %>% 
  melt()%>% 
  ggplot(aes(x=Var1,y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation") -> 
  analyzedData

ggsave("analyzedData.pdf")

#3

library(quantreg)

##OLS

x <- data.frame(metalc,fuelc,crudeoilc,naturalgasc,coalc,fuell,crudeoill,
           naturalgasl,coall)

OLS <- lm(metalc~fuelc + crudeoilc + naturalgasc + coalc + fuell + crudeoill +
          naturalgasl + coall, data = x)
summary(OLS)

names(OLS)
resid = OLS$residuals
resid

##Quant regression

qseq = seq(0.1,0.9,by=0.1)

quant_fit = rq(metalc~fuelc + crudeoilc + naturalgasc + coalc + fuell + crudeoill +
                 naturalgasl + coall,data = x,tau=qseq)
summary(quant_fit)

##Plot OLS

x %>% map(~ggplot(x, aes(x=metalc, y=.))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE))

##Plot Quant

plot(summary(quant_fit), parm=c(2), level=0.95, main="MetalC Quant Regression")
title(xlab="Quantile",ylab="Estimated effect")

##4

library(glmnet)

xlasso <- model.matrix(metalc~fuelc + crudeoilc + naturalgasc + coalc + fuell + crudeoill +
                    naturalgasl + coall)[, -1]
ylasso <- x$metalc

grid <- 10^ seq(10, -2, length = 100)

train <- sample (1: nrow (xlasso) , nrow (xlasso)/2)
test <- (-train)
y.test <- ylasso[test]

cv.out <- cv.glmnet(xlasso[train , ], ylasso[ train ], alpha = 1)
plot (cv.out)
bestlam <- cv.out$lambda.min

out <- glmnet (xlasso , ylasso , alpha = 1, lambda = grid )
lasso.coef <- predict (out , type = "coefficients",
                            s = bestlam ) [1:9, ]
lasso.coef #lambda is therefore 9-2=7

