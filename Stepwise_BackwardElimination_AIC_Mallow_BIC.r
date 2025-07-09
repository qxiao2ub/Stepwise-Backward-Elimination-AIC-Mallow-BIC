---
title: ""
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



--------------------------------------------------------------------------------------

## Problem 1 

Use the prostate data under the faraway r library, data(prostate,package="faraway"). Use the lpsa as the response and the other variables as predictors. 

Implement the following variable selection methods to determine the best model: (25 points, 5 points each)

a-) Backward elimination

```{r}
data(prostate,package="faraway")
```

```{r}
library(olsrr)
```


```{r}
p1.reg=lm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)
a=ols_step_backward_p(p1.reg)
a
```

\textcolor{blue}{Using Backward elimination p-value is small and R value is 0.809 and R-square is 0.654}

b-) AIC

```{r}
b=ols_step_both_aic(p1.reg)
b
```

\textcolor{blue}{Using AIC R value is 0.803 and R-square is 0.644}

c-) Adjusted $R_{a}^2$

```{r}
c=ols_step_forward_adj_r2(p1.reg)
c
```

\textcolor{blue}{Using adjusted r square R value is 0.809 and R-square is 0.654}


d-) Mallow's $C_{p}$

```{r}
full_model=lm(lpsa~.,data = prostate)
d=ols_mallows_cp(p1.reg,full_model)
d
```

e-) BIC

```{r}
e=ols_step_both_sbic(p1.reg)
e
```

\textcolor{blue}{Using BIC R value is 0.798 and R-square is 0.637}

\textcolor{blue}{We can see both backward elimination and adjusted r square have best performances as they have good r square value, RMSE, MAE and MSE.}

## Problem 2

Refer to the SENIC data set. Length of stay (Y) is to be predicted, and the pool of potential predictor variables includes all other variables in the data set except medical school affiliation and region. It is believed that a model with $log(Y)$  as the response variable and the predictor variables in first-order terms with no interaction terms will be appropriate. Consider cases 57-113 to constitute the model-building data set to be used for the following analyses.(40 points, 10 points each)

a-) Obtain the three best subsets according to the $C_p$ criterion, Which of these subset models appears to have the smallest bias?

#\textcolor{blue}{Except for medical school affiliation and region, we have other 8 decision variables from age to available facilities and services. So we can split 8 decision variables into three }


```{r select rows 57-113}
SENIC0 <- read.csv("/cloud/project/SENIC.csv")
SENIC.1=SENIC0[57:113,]
```

```{r use library for part a}
library(leaps)
```


```{r }
#builtup_log(Y) regression_model._nvmax=3 return up to all 8-variables model
SENIC.1$log = log(SENIC.1$Length.of.stay)
regfit1=regsubsets(SENIC.1$log~SENIC.1$Age + SENIC.1$Infection.risk +SENIC.1$Routine.culturing.ratio+SENIC.1$Routine.chest.X.ray.ratio+SENIC.1$Number.of.beds+SENIC.1$Average.daily.census+SENIC.1$Number.of.nurses+SENIC.1$Available.facilities.and.services, data=SENIC.1, nvmax = 8)
summary(regfit1)
```


```{r analyze for cp}
summary(regfit1)$cp
```

\textcolor{blue}{So the highest Cp value is 16.2329 that is for a model with only one predictor variable, the best regression model is produced using average daily census. The second highest Cp value is 9, so for a model with all 8 predictor variables, the best regression model is produced using all 8 prediction variables. The third highest Cp value is 7.479 that is for a model with two predictor variables, the best regression model is produced using routine chest x-ray ratio and average daily census.}


b-) Implement the following variable selection methods to determine the best model: Backward elimination and BIC methods.

```{r}
#builup regression model as log(Y)
p2.reg=lm(SENIC.1$log~SENIC.1$Age + SENIC.1$Infection.risk +SENIC.1$Routine.culturing.ratio+SENIC.1$Routine.chest.X.ray.ratio+SENIC.1$Number.of.beds+SENIC.1$Average.daily.census+SENIC.1$Number.of.nurses+SENIC.1$Available.facilities.and.services,data=SENIC.1)
summary(p2.reg)
```

```{r}
#using backward elimination
a=ols_step_backward_p(p2.reg)
a
```

\textcolor{blue}{Using backward elimination, R-squared value is 0.551}

```{r}
#using BIC method
b2=ols_step_both_sbic(p2.reg)
b2
```

\textcolor{blue}{Using BIC R-squared value is 0.519}

\textcolor{blue}{Conclusion: as we compare R-squared value using backward elimination and BIC methods, the backward limination has better regression model as it has higher R-squared value.}

c-) The regression model identified as best in part b-) is to be validated by means of the validation data set consisting of cases 1-56.  Fit the regression model identified in part b-) as best to the validation data set. Compare the estimated regression coefficients and their estimated standard deviations with those obtained in Part b-). 

```{r select rows 1-56}
SENIC.2=SENIC0[1:56,]
#make log columns
SENIC.2$log = log(SENIC.2$Length.of.stay)
#fit model using 1-56 data
c2=predict(p2.reg,data=SENIC.2)
c2
```

```{r}
#add c2 to last column
SENIC.2$fit=head(c2,56)
```


```{r}
#calculate standard deviation of estimates
sd(c2)
```
```{r}
#calculate standard deviation in parb (b)
sd(SENIC.1$log)
```

\textcolor{blue}{The part (b) standard deviation is 0.1785024, this estimated one is 0.1339326}

```{r}
#builup estimated fitted regression model as log(Y)
p21.reg=lm(SENIC.2$fit~SENIC.2$Age + SENIC.2$Infection.risk +SENIC.2$Routine.culturing.ratio+SENIC.2$Routine.chest.X.ray.ratio+SENIC.2$Number.of.beds+SENIC.2$Average.daily.census+SENIC.2$Number.of.nurses+SENIC.2$Available.facilities.and.services,data=SENIC.2)
summary(p21.reg)
```



d-) Also compare the error mean squares and coefficients of multiple determination. Does the model fitted to the validation data set yield similar estimates as the model fitted to the model-building data set?

```{r}
#calculate mean square error MSE for fitted model
mean(p21.reg$residuals^2)
```

```{r}
#calculate mean square error MSE for original model
mean(p2.reg$residuals^2)
```

\textcolor{blue}{We can see p-value is greater than 5 percent and coefficients are not significant, so the model fitted to the validation data set does not yield similar estimates as the model fitted to the modeling-building data set.}


## Problem 3

Using the teengamb dataset under the faraway r library, data(teengamb,package="faraway") with gamble as the response and the other variables as predictors. Use the stepwise both ways method to select the best model and ensure that all variables in the final model is significant. (15 points) 

```{r}

library(faraway)
data(teengamb)
```



```{r}

p3.reg=lm(gamble~.,data=teengamb)
summary(p3.reg)
```

```{r}
ols_step_both_adj_r2(p3.reg,details=TRUE)
```

```{r}
ols_step_both_aic(p3.reg)
```

```{r}
ols_step_both_p(p3.reg)
```

```{r}
ols_step_both_r2(p3.reg)
```

```{r}
ols_step_both_sbc(p3.reg)
```

```{r}
ols_step_both_sbic(p3.reg)
```

\textcolor{blue}{So we can see using stepwise regression, stepwise SBC regression and stepwise SBIC regression turn out all decision avariables are significant.}

## Problem 4

Refer to the Lung pressure Data. Increased arterial blood pressure in the lungs frequently leads to the development of heart failure in patients with chronic obstructive pulmonary disease (COPD). The standard method for determining arterial lung pressure is invasive, technically difficult, and involves some risk to the patient. Radionuclide imaging is a noninvasive, less risky method for estimating arterial pressure in the lungs. To investigate the predictive ability of this method, a cardiologist collected data on 19 mild-to-moderate COPD patients. The data includes the invasive measure of systolic pulmonary arterial pressure (Y) and three potential noninvasive predictor variables. Two were obtained by using radionuclide imaging emptying rate of blood into the pumping chamber or the heart ($X_1$) and ejection rate of blood pumped out of the heart into the lungs ($X_2$) and the third predictor variable measures blood gas ($X_3$). (20 points)

a-) Find the best regression model by using  first-order terms and  the cross-product term. Ensure that all variables in the model are significant at 5%. (10 points)

```{r}
PressDat = read.csv("/cloud/project/Lung Pressure.csv")
```

```{r}
p3.reg=lm(PressDat$Y~.,data=PressDat)
summary(p3.reg)
```

```{r}
#Perform a Box-Cox transformation analysis and decide if Y needs to be transformed.
library(MASS)
boxcox(p3.reg,lambda=seq(-5,5,by=.1))
```
```{r}
boxcox(p3.reg, lambda = seq(-.9,.5,by=.01))
```

\textcolor{blue}{We can see the maximum loglikely hood close to lambda=0, so a log(Y) transformation is needed.}

```{r}
#log transformation
PressDat$'log(Y)' = log(PressDat$Y)
```

```{r}
p3.reg_1=lm(PressDat$`log(Y)`~.,data = PressDat)
summary(p3.reg_1)
```

\textcolor{blue}{Following using cross-product term for regression model:}

```{r}
p3.reg_cro=lm(PressDat$Y ~poly(PressDat$X1 ,PressDat$X2 ,PressDat$X3), data = PressDat)
summary(p3.reg_cro)
```

```{r}
#boxcox to find Y transformation
boxcox(p3.reg_cro,seq(-5,5,by=.1))

```

```{r}
boxcox(p3.reg_cro,seq(-.9,.7,by=.005))
```

\textcolor{blue}{Maximum loglikely hood falls between -.5 and 0, so we transfer Y to 1/sqrt(Y) and log(Y) to see which one is better. As we have done transformation for log(Y) in previous step, so now just need to transform 1/sqrt(Y).}

```{r}
#transform to 1/sqrt(Y)
PressDat$'1/sqrt(Y)' = 1/sqrt(PressDat$Y)
```

```{r}
#use log(Y) as Y and do cross-product
p3.reg_cro_log=lm(PressDat$`log(Y)`~poly(PressDat$X1,PressDat$X2,PressDat$X3),data = PressDat)
summary(p3.reg_cro_log)
```

```{r}
#use 1/sqrt(Y) as Y and do cross-product term
p3.reg_cro_sqr=lm(PressDat$`1/sqrt(Y)`~poly(PressDat$X1,PressDat$X2,PressDat$X3),data=PressDat)
summary(p3.reg_cro_sqr)
```

b-)	Obtain the residuals and plot them separately against Y and each of the three predictor variables. On the basis of these plots. should any further modification of the regression model be attempted? (5 points)

```{r}
#X1 as X axis
p3.res=p3.reg$residuals
plot(PressDat$X1,p3.res)
```

```{r}
plot(PressDat$X2,p3.res)
```

```{r}
plot(PressDat$X3,p3.res)
```

```{r}
#shapiro.test
shapiro.test(p3.res)
```

```{r}
par(mfrow=c(2,2))
plot(p3.reg)
```

\textcolor{blue}{From Q-Q residual plot we do not see many outliers (only point 7, 8 and 13 are outliers), and by Shapiro test the p-value is 0.21>0.05 so it is not significant then stays at $H_0$ hypothesis that residuals stay normal distributed. Based on these two pespectives there is no further modification of the regression model need attempted.}

c-)	Prepare a normal probability plot of the residuals. Also obtain the coefficient of correlation between the ordered residuals and their expected values under normality. Does the normality assumption appear to be reasonable here? (5 points)

```{r}
par(mfrow=c(2,2))
plot(p3.reg)
```

\textcolor{blue}{A normal probability plot of the residuals as shown above.}

```{r}
#get the expected value using prediction
a=predict(p3.reg,newdata=PressDat)
a
```

```{r}
#look at residual data
b=p3.res
b
```

```{r}
#get the correlation
cor_res=cor(a,b,method=c("pearson","kendall","spearman"))
cor_res
```

\textcolor{blue}{We can see the correlation between the ordered residuals and their expected values under normality is very week, so the normality assumption appears to be reasonable here.}

