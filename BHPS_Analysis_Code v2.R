


# load required packages
if(!require(dplyr)) install.packages("dplyr", repos = "")
if(!require(ggplot2)) install.packages("ggplot2", repos = "")
if(!require(forcats)) install.packages("forcats", repos = "")
if(!require(stargazer)) install.packages("stargazer", repos = "")

library(dplyr)
library(ggplot2)
library(forcats)
library(stargazer)




## Read a csv file
bhps_data <- read.csv("https://raw.github.com/vonpaumgartten/BHPS/master/BHPS_Raw_Data.csv")

## Dimesions of the dataset
dim(bhps_data) ## 3500 rows and 11 columns

## Columns in the dataset
str(bhps_data)

## Summary of the dataset
summary(bhps_data)

##Removing all the records with negative values
bhps_data = bhps_data[!rowSums(bhps_data < 0), ]

## Dimesions of the dataset
dim(bhps_data) ## 3491 rows and 11 columns



## Transforming the numerically coded variables into factors
bhps_data$sex <- as.factor(bhps_data$sex)
bhps_data$avote <- as.factor(bhps_data$avote)
bhps_data$aregion <- as.factor(bhps_data$aregion)
bhps_data$ajbstat <- as.factor(bhps_data$ajbstat)
bhps_data$ajbterm <- as.factor(bhps_data$ajbterm)
bhps_data$atenure <- as.factor(bhps_data$atenure)


## Merge the categories to ensure no more than 4 categories per variable

bhps_data$avote <- fct_collapse(bhps_data$avote, Others = c("4","5","6","7","8","10","11"))
bhps_data$aregion <- fct_collapse(bhps_data$aregion, Others = c("1","2","5","6","7","8","9","10","11","12","13","14","15","16","17"))
bhps_data$ajbstat <- fct_collapse(bhps_data$ajbstat, Others = c("1","3","4","5","7","8","10"))
bhps_data$atenure <- fct_collapse(bhps_data$atenure, Others = c("4","5","6","7","8"))

## Summary of the dataset
summary(bhps_data)


#Checking the levels for each of the Factor variables
levels(bhps_data$sex)
levels(bhps_data$avote)
levels(bhps_data$aregion)
levels(bhps_data$ajbstat)
levels(bhps_data$ajbterm)
levels(bhps_data$atenure)




## Box-plots for Categorical variables
box_plot_sex <- ggplot(bhps_data, aes(x=sex, y=afihhyr, group=sex))
box_plot_sex + geom_boxplot(outlier.colour = "red") +
  ggtitle("Annual Household Income by Sex")

box_plot_avote <- ggplot(bhps_data, aes(x=avote, y=afihhyr, group=avote))
box_plot_avote + geom_boxplot(outlier.colour = "red") +
  ggtitle("Annual Household Income by Political Party Supported")

box_plot_aregion <- ggplot(bhps_data, aes(x=aregion, y=afihhyr, group=aregion))
box_plot_aregion + geom_boxplot(outlier.colour = "red",outlier.size = 1) +
  ggtitle("Annual Household Income by Region,Metropolitan Area")

box_plot_ajbstat <- ggplot(bhps_data, aes(x=ajbstat, y=afihhyr, group=ajbstat))
box_plot_ajbstat + geom_boxplot(outlier.colour = "red",outlier.size = 1) +
  ggtitle("Annual Household Income by Current labour force status")

box_plot_ajbterm <- ggplot(bhps_data, aes(x=ajbterm, y=afihhyr, group=ajbterm))
box_plot_ajbterm + geom_boxplot(outlier.colour = "red",outlier.size = 1) +
  ggtitle("Annual Household Income by Current Job")

box_plot_atenure <- ggplot(bhps_data, aes(x=atenure, y=afihhyr, group=atenure))
box_plot_atenure + geom_boxplot(outlier.colour = "red",outlier.size = 1) +
  ggtitle("Annual Household Income by Housing tenure")


### Box plot for all categorical variables in a single view
par(mfrow=c(2,2))
boxplot(afihhyr ~ sex, data=bhps_data, main="Annual Household Income by Sex")
boxplot(afihhyr ~ ajbterm, data=bhps_data, main="Annual Household Income by Current job")
boxplot(afihhyr ~ atenure, data=bhps_data, main="Annual Household Income by Housing tenure")
boxplot(afihhyr ~ ajbstat, data=bhps_data, main="Annual Household Income by Current labour force status")


par(mfrow=c(2,1))
boxplot(afihhyr ~ avote, data=bhps_data, main="Annual Household Income by Political Party Supported")
boxplot(afihhyr ~ aregion, data=bhps_data, main="Annual Household Income by Region")



## Scatter-plots for Continuous variables

ggplot(bhps_data, aes(x = aage, y = afihhyr)) + geom_point() +
  labs(title = "Annual Household Income by Age")

ggplot(bhps_data, aes(x = AJBHRS, y = afihhyr)) + geom_point() +
  labs(title = "Annual Household Income by Hours worked per week")

ggplot(bhps_data, aes(x = ancars, y = afihhyr)) + geom_point() +
  labs(title = "Annual Household Income by Cars available")

ggplot(bhps_data, aes(x = ahhsize, y = afihhyr)) + geom_point() +
  labs(title = "Annual Household Income by Persons in household")


## ANOVA Tests

sex_anova <- aov(afihhyr~sex, data = bhps_data)
avote_anova <- aov(afihhyr~avote, data = bhps_data)
aregion_anova <- aov(afihhyr~aregion, data = bhps_data)
ajbstat_anova <- aov(afihhyr~ajbstat, data = bhps_data)
ajbterm_anova <- aov(afihhyr~ajbterm, data = bhps_data)
atenure_anova <- aov(afihhyr~atenure, data = bhps_data)



summary(sex_anova)
summary(avote_anova)
summary(aregion_anova)
summary(ajbstat_anova)
summary(ajbterm_anova)
summary(atenure_anova)




## Linear Regression Model:

# build linear regression model on full data
Model_1 <- lm(afihhyr ~sex + aage + AJBHRS + ancars + avote + aregion + ajbstat + 
                ajbterm + ahhsize + atenure, data=bhps_data)  
summary(Model_1)

# Call:
#   lm(formula = afihhyr ~ sex + aage + AJBHRS + ancars + avote + 
#        aregion + ajbstat + ajbterm + ahhsize + atenure, data = bhps_data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -34289  -6779  -1660   4694 158372 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    5272.39    2541.12   2.075 0.038076 *  
#   sex2           1601.93     453.47   3.533 0.000417 ***
#   aage             55.55      17.90   3.103 0.001928 ** 
#   AJBHRS          176.61      20.07   8.798  < 2e-16 ***
#   ancars         5886.35     278.84  21.110  < 2e-16 ***
#   avote2        -1282.78     487.81  -2.630 0.008585 ** 
#   avote3           14.11     668.21   0.021 0.983150    
#   avoteOthers   -2394.20     654.60  -3.657 0.000258 ***
#   aregion3       1279.74     530.26   2.413 0.015854 *  
#   aregion4      -2716.15     749.34  -3.625 0.000293 ***
#   aregion18      2328.34     737.82   3.156 0.001615 ** 
#   ajbstat2      -4721.72    2058.12  -2.294 0.021839 *  
#   ajbstat6      -1994.85    2472.82  -0.807 0.419890    
#   ajbstat9      -7851.96    3779.95  -2.077 0.037850 *  
#   ajbterm2       -253.52     942.72  -0.269 0.788007    
#   ajbterm3        988.06    1154.53   0.856 0.392159    
#   ahhsize        1733.34     175.19   9.894  < 2e-16 ***
#   atenure2       2745.80     635.62   4.320  1.6e-05 ***
#   atenure3      -2549.28     858.24  -2.970 0.002995 ** 
#   atenureOthers  -588.51     875.44  -0.672 0.501472    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 11900 on 3471 degrees of freedom
# Multiple R-squared:  0.2426,	Adjusted R-squared:  0.2384 
# F-statistic:  58.5 on 19 and 3471 DF,  p-value: < 2.2e-16

#We can see that p-value for all the categories of ajbterm are >0.5. 
#Hence, we will discard the variable and re-run the model




## Step-wise Regression Model
Model_1_best <- lm(afihhyr ~sex + aage + AJBHRS + ancars + avote + 
                     aregion + ajbstat + ahhsize + atenure, data=bhps_data)  
summary(Model_1_best)


# Call:
#   lm(formula = afihhyr ~ sex + aage + AJBHRS + ancars + avote + 
#        aregion + ajbstat + ahhsize + atenure, data = bhps_data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -34308  -6788  -1676   4667 158358 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    5262.38    2517.22   2.091 0.036641 *  
#   sex2           1606.22     453.35   3.543 0.000401 ***
#   aage             55.61      17.87   3.112 0.001874 ** 
#   AJBHRS          176.79      19.82   8.919  < 2e-16 ***
#   ancars         5884.96     278.78  21.110  < 2e-16 ***
#   avote2        -1282.38     487.69  -2.630 0.008588 ** 
#   avote3           19.62     667.81   0.029 0.976567    
#   avoteOthers   -2416.47     653.92  -3.695 0.000223 ***
#   aregion3       1280.25     530.17   2.415 0.015795 *  
#   aregion4      -2719.85     749.19  -3.630 0.000287 ***
#   aregion18      2370.30     736.09   3.220 0.001293 ** 
#   ajbstat2      -4719.02    2054.18  -2.297 0.021662 *  
#   ajbstat6      -2056.85    2465.12  -0.834 0.404122    
#   ajbstat9      -7232.52    3712.79  -1.948 0.051495 .  
#   ahhsize        1734.27     175.15   9.902  < 2e-16 ***
#   atenure2       2759.24     634.24   4.350  1.4e-05 ***
#   atenure3      -2538.44     857.95  -2.959 0.003110 ** 
#   atenureOthers  -556.44     874.59  -0.636 0.524666    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 11890 on 3473 degrees of freedom
# Multiple R-squared:  0.2424,	Adjusted R-squared:  0.2387 
# F-statistic: 65.36 on 17 and 3473 DF,  p-value: < 2.2e-16

## We can observe that all the variables have p-value less than 0.05. 
## Hence, all the variables are significant for our model.





########### Model's using Log Transformation ###############

# build linear regression model on full data
Model_2_log <- lm(log(afihhyr) ~sex + log(aage) + log(AJBHRS) + ancars + avote + aregion + 
                    ajbstat + ajbterm + log(ahhsize) + atenure, data=bhps_data)  
summary(Model_2_log)


# Call:
#   lm(formula = log(afihhyr) ~ sex + log(aage) + log(AJBHRS) + ancars + 
#        avote + aregion + ajbstat + ajbterm + log(ahhsize) + atenure, 
#      data = bhps_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.0143 -0.2589  0.0176  0.2835  1.7269 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    8.25535    0.14686  56.213  < 2e-16 ***
#   sex2           0.05609    0.01694   3.310 0.000942 ***
#   log(aage)      0.06427    0.02436   2.638 0.008378 ** 
#   log(AJBHRS)    0.23357    0.01802  12.962  < 2e-16 ***
#   ancars         0.22188    0.01078  20.591  < 2e-16 ***
#   avote2        -0.02417    0.01872  -1.292 0.196566    
#   avote3         0.01295    0.02562   0.505 0.613244    
#   avoteOthers   -0.09089    0.02516  -3.612 0.000308 ***
#   aregion3       0.06526    0.02033   3.210 0.001341 ** 
#   aregion4      -0.08203    0.02874  -2.854 0.004337 ** 
#   aregion18      0.10814    0.02830   3.822 0.000135 ***
#   ajbstat2      -0.04462    0.07898  -0.565 0.572097    
#   ajbstat6       0.16630    0.09594   1.733 0.083136 .  
#   ajbstat9      -0.27415    0.14517  -1.889 0.059040 .  
#   ajbterm2      -0.06877    0.03649  -1.885 0.059539 .  
#   ajbterm3       0.08337    0.04439   1.878 0.060430 .  
#   log(ahhsize)   0.29930    0.01863  16.063  < 2e-16 ***
#   atenure2       0.13062    0.02410   5.420 6.38e-08 ***
#   atenure3      -0.19679    0.03277  -6.004 2.12e-09 ***
#   atenureOthers -0.06774    0.03337  -2.030 0.042426 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4561 on 3471 degrees of freedom
# Multiple R-squared:  0.3275,	Adjusted R-squared:  0.3238 
# F-statistic: 88.96 on 19 and 3471 DF,  p-value: < 2.2e-16

## We tried to remove the isignificant variables but the R Squared decreased further,
## Hence, we conclude this as the significant model



### Residual Plots for the Final Model
plot(fitted(Model_2_log), residuals(Model_2_log),abline(0, 0)) 


## Comparing all the 3 models
stargazer(Model_1, Model_1_best, Model_2_log,type="text",
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE)