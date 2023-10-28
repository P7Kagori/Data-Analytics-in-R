---
title: "Heteroscedasticity in Data Analysis in R.rmd"
author: "Paul N. Kagori, MSc. Applied Mathematics, University of Debrecen, Hungary"
date: "2023-10-23"
output:
    pdf_document: default
---
```{r}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
```
##Required Libraries
```{r}
library(ggplot2)
library(AER)
library(dplyr)
library(knitr)
library(tidyverse) #data wrangling
library(foreign)   #read.dta()
library(MASS)
```
#HETEROSCEDASTICITY

##Introduction

The main assumption of linear regression is that residuals are normally distributed with zero mean and constant variance. That is, Given a linear regression model:
$$
y_{i} = \beta_{o} +\beta_{i} x_{i1} +\beta_{2} x_{i2} +\cdots + \beta_{r} x_{ir} + \xi_i, \text{where}, \quad i=1, 2, 3,\cdots, n. \quad \xi_i= \hat{y_i} - y_i
$$
By definition, the residual is given by
$$
\xi_i = \hat{y_i}- y_i
$$
###The Data Exploration- House Price Data

In this exercise, we consider the House prices data
```{r}
load("EconData.RData")
head(hprice1) # to view the first 5 rows of the data
colnames(hprice1)
dim(hprice1) ## The dimension of the data
str(hprice1) # Get the view of structure
```
#First Linear Model

We want to predict the price of the house,using lotsize,fit squared(sqft), bedrooms (bdrms) 
```{r}

price_lm1 <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
summary(price_lm1)
```
###Model 1 Results

Notice that from the model,the intercept, and the number of bedrooms are not significant. In other words, they do not statistically influence the price of the house. But checking Residual Sum of Squares (R^2), to see how well the reggressors fits the model. We have
$$
R^2 = .6724
$$
Which shows that Lotsize, Sqrft accounts for 67.24% of the house price. 

###Model 1 Residuals
```{r}
res1 <- residuals(price_lm1) ## Obtaining the residuals of the model1
##Ploting the fitted values Vs actual values
houseprice <-hprice1$price
dframe <-data_frame(houseprice,res1)
dframe %>%
  ggplot(aes(houseprice,res1)) +geom_point() + geom_smooth() + ggtitle("Actual House Price vs Predicted Model1 House Prices")
```

Checking on above plot, its obvious that residual error increases linearly with increase on the price. Hence its not constant and thus violates the main assumption of linear regression (i.e Homoscedasticity)

####Dropping the Non-significant Coefficient

We now drop the non-significant parameters (Intercept and bdrms)and run a new regression analysis. We regress price against lotsize & sqrft and remove the intercept.

```{r}
price_lm2 <- lm(price ~ lotsize + sqrft - 1, data = hprice1)
summary(price_lm2)
```
Notice that the
$$
R^2 = 0.9636
$$ 
Hence, Lotsize and sqrft explains about 96.36% of the house price. Its a near perfect regression fit.


##Checking the Residuals of Model2 & Plots

```{r}
res2 <- residuals(price_lm2)
dframe2 <-data_frame(houseprice,res2)
dframe %>%
  ggplot(aes(houseprice,res2)) +geom_point() + geom_smooth() + ggtitle("Actual House Price vs Predicted Model2 House Prices") ## Still there is heteroscedasticity
```

###COEFFICIENTS OF THE MODEL2
```{r}
coef(price_lm2) ## Checking the coefficients of the bivariate model
confint(price_lm2, level = 0.95) ## The 95% confidence interval of the coefficients
```



#TESTING HETEROSCEDASTICITY


Other than graphical approach, we can use 4 tests to check for heteroscedasticity in regression analysis.

## 1. RAMSEY REGRESSION SPECIFICATION ERROR TEST, 1969


In this test, we consider a regression of residuals on the 

$$
\hat{y}^2, \hat{y}^3, \hat{y}^4, \cdots, \text{and so on}
$$. 
Notice that the model that we run is:
$$
\xi_i = \alpha_0 +\alpha_1 \hat{y}^2 +\alpha_2\hat{y}^3 +, \cdots, \alpha_m \hat{y}^m +\rho_i \quad , \quad i = 1,2,\cdots, n 
$$
Under Null hypothesis, we have 
$$
H_o : \alpha_0 = \alpha_1 = \alpha_2=,\cdots, =\alpha_m  = 0 \quad \text{ coefficient are not signficant}\\
H_1 :\text{atleast one} \quad \alpha_i 's\quad  \text{Is not equal to zero or atleast one coefficient is significant}
$$


###Estimated Prices

From the model price_lm2, we obtain the predicted predicted value of the house prices 
```{r}
pre_price <-predict(price_lm2) # column vector for the predicted house prices
```


###We now have the following model under Ramsey Reset

```{r}
ram2 <- lm(res2 ~ I(pre_price^2) + I(pre_price^3)+ I(pre_price^4))
summary(ram2)
```

The pvalue is .04373 is less that level of significance = .05. Hence we reject the null hypothesis and conclude that coefficients are significant. Hence, the original model suffers from misspecification. Hence original model is heteroscedastic. 



## 2. Breusch-Pagan Test, 1969



The original model is:
$$
y = \beta_0 + \beta_1 x_1  + \beta_2 x_2 + \beta_3 x_3 + ,
\cdots, \beta_r x_r + \eta_i
$$

The test model is:
$$
\xi_i^2 = \alpha_0 +\alpha_1 x_1 +\alpha_2 x_2 +, \cdots, \alpha_m x_m +\rho_i
$$

The test of hypothesis is done as follows:
$$
H_o : \text{The original model is Homoscedastic}\\

H_1 : \text{The original model is Heteroscedastic}
$$
We then carry out ANOVA test and assume the Errors are have a Gaussian distribution. Under the null hypothesis, we have to accept that:

$$
\alpha_1 = \alpha_2 = \alpha_3 =, \cdots = \alpha_r = 0
$$


###Test Model

```{r}
price_lm2res <- lm(res2^2 ~ lotsize + sqrft, data = hprice1)
summary(price_lm2res)
```

###Checking the ANOVA


```{r}
anova(price_lm2res)
```
Again, from ANOVA, the lotsize and sqrft are significant.Hence the coefficients in the test models are not all zero and we should reject Ho, and accept H1. Meaning the original model is heteroscedastic. 

## 3. White Test, 1980

In this case, we regress square of residuals on the original reggressors, their squares, and their cross products. That is

$$
\text{Origonal Model} : y = \textit{lotsize} \quad + \textit{sqrft} +\xi
$$

If We consider the test model. Let x1 = lotsize, x2= sqrft:
$$
\xi^2 = \alpha_o + \alpha_1 x_1  + \alpha_2 x_2 + \alpha_3 x_1^2  + \alpha_4 x_2^2 + \alpha_5 x_1 x_2
$$
The coefficient of determination is:
$$ 
R^2 \quad\text{assumed to be close to 1}, \quad\text{and} \quad nR^2 \sim \chi_r ^2 
$$

Hence,we have the following hypothesis to test:
$$
H_o : \text{The original model is Homoscedastic} \\
H_1 : \text{The original model is Heteroscedastic}
$$

If the original model is Homoscedastic, then
$$
R^2 \approx 0
$$

```{r}
n <- dim(hprice1)[1] # Get the sample size
```

###The Model
```{r}
price_lm2white <- lm(res2^2 ~ lotsize + sqrft + I(lotsize^2) + I(sqrft^2) +
I(lotsize * sqrft), data = hprice1)
summary(price_lm2white)
```

#### We obtain the P-value
```{r}
SQ2w <- summary(price_lm2white)$r.squared ## R^2 of the above model 
df2w <- summary(price_lm2white)$fstatistic['numdf'] ## Degrees of freedom
pchisq(n*SQ2w, df2w, lower.tail = F) ##We only want the upper tail
```

Definitely, the value 0.000007652783 is close to zero. We conclude that original model is Homoscedastic

#####Checking the plot

```{r}
plot(hprice1$lotsize,res2, color = "blue")
```


