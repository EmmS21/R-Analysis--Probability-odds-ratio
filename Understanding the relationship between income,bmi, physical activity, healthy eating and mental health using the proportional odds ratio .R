install.packages("ggplot2")
install.packages("mice")
install.packages("effects")
install.packages("DMwR")
library(ggplot2)
library(MASS)
library(mice)
library(data.table)
library(dplyr)
library(effects)
library(DMwR)
load("~/Documents/R Stats Assignments/brfss2013.RData")
#using multi imputationlinear regression to predict values of pa1min_ using the mice library
brfss2 <- brfss2013[,c('X_bmi5','X_drnkmo4','X_frutsum','X_vegesum','pa1min_')]
#mean too simplistic + won't capture data variability
#imputing using multiple imputation deterministic regression
imp_model <- mice(brfss2,method="norm")
# Replace the fitted coefficients with the pooled
fillna_fun <- function(data,columns){
  df <- setNames(data.frame(rowMeans(squeeze(imp_model$imp[[columns]], bounds = c(0,max(brfss2013[[columns]]))))),"col2")
  brf <- setNames(data.frame(data[[columns]]),"col2")
  brf$col1 <- rownames(brf)
  df$col1 <- rownames(df)
  setDT(brf)[df,col2 :=i.col2,on=.(col1)]
  brf$col2
}
brfss2013$pa1min_ <- fillna_fun(brfss2,"pa1min_")
brfss2013$X_bmi5 <- fillna_fun(brfss2,"X_bmi5")
brfss2013$X_drnkmo4 <- fillna_fun(brfss2,"X_drnkmo4")
brfss2013$X_frutsum <- fillna_fun(brfss2,"X_frutsum")
brfss2013$X_vegesum <- fillna_fun(brfss2,"X_vegesum")
#I want to convert income2 column to strings for to use grepl to identify income levels
brfss2013$income2 <- as.character(brfss2013$income2)
brfss2013$X_bmi5 <- brfss2013$X_bmi5/100
brfss2013$healtheat <- (brfss2013$X_frutsum+brfss2013$X_vegesum)/100
#cut is designed to take a numeric vector and split it into bins on a set of breakpoints- more simplicity than if else statements
labels <- c('Excellent','Good','Ok','Bad','Very Bad')
breaks <- c(0,5,10,15,25,10000)
bmiLabs <- c('10','20','30','40','50','60','>60')
bmiBreaks <-c(0,10,20,30,40,50,60,10000)
activLabs <-c('0-200','200-500','500-1000','1000-2000','2000-4000','4000-10000','>10000')
activBreaks <-c(0,200,500,1000,2000,4000,10000,100000)
brfss2013 <- brfss2013 %>%
  mutate(mentalHealth = cut(menthlth,breaks=breaks,labels=labels,include.lowest=TRUE)) %>%
  mutate(bmiLev = cut(X_bmi5, breaks=bmiBreaks,labels=bmiLabs,include.lowest = TRUE)) %>%
  mutate(physLev = cut(pa1min_, breaks=activBreaks,labels=activLabs,include.lowest = TRUE)) %>%
  mutate(incomeLev = case_when(grepl("15|20",income2)~"0-$20k",
                               grepl("25|35",income2)~"25-$35k",
                               grepl("50",income2)~"35-$50k",
                               income2 %in% "Less than $75,000" ~ "50-$75k",
                               grepl("more",income2)~">$75k"
  )) 
#visualizations
#convert income back to factor
brfss2013$mentalHealth <- forcats::fct_explicit_na(brfss2013$mentalHealth, na_level = "Missing")
brfss2013$incomeLev <- as.factor(brfss2013$incomeLev) 
brfss2013 %>%
  add_count(incomeLev) %>%
  rename(count_inc = n) %>% 
  count(incomeLev, mentalHealth, count_inc) %>%
  rename(count_mentalHealth = n) %>% 
  mutate(percent= count_mentalHealth / count_inc) %>%
  mutate(incomeLev = factor(incomeLev,
                            levels=c('0-$20k','25-$35k','35-$50k','50-$75k','>$75k')))%>%
  ggplot(aes(x= incomeLev,
             y= count_mentalHealth,
             group= mentalHealth)) + 
  xlab('Annual Income')+ylab('Number of People')+
  geom_bar(aes(fill=mentalHealth), 
           stat="identity",na.rm=TRUE)+ 
  # Using the scales package does the percent formatting for you
  geom_text(aes(label = scales::percent(percent)),position = position_stack(vjust = 0.5))+
  theme_minimal()
#filling na values for income level column
brfss <- brfss2013[,c('incomeLev','healtheat','X_age_g','employ1','renthom1','sex','physLev')]
ordered_brfss <-mice(brfss, m=1, method='polr', maxit=1)
temp <- complete(ordered_brfss)
brfss2013$incomeLev <- temp$incomeLev
#visualise after imputation
brfss2013 %>%
  add_count(incomeLev) %>%
  rename(count_inc = n) %>% 
  count(incomeLev, mentalHealth, count_inc) %>%
  rename(count_mentalHealth = n) %>% 
  mutate(percent= count_mentalHealth / count_inc) %>%
  mutate(incomeLev = factor(incomeLev,
                            levels=c('0-$20k','25-$35k','35-$50k','50-$75k','>$75k')))%>%
  ggplot(aes(x= incomeLev,
             y= count_mentalHealth,
             group= mentalHealth)) + 
  xlab('Annual Income')+ylab('Number of People')+
  geom_bar(aes(fill=mentalHealth), 
           stat="identity",na.rm=TRUE)+ 
  # Using the scales package does the percent formatting for you
  geom_text(aes(label = scales::percent(percent)),position = position_stack(vjust = 0.5))+
  theme_minimal()
#we need to fit the data into the logistic model
brfss2_model = polr(mentalHealth ~ incomeLev+bmiLev+X_drnkmo4+healtheat+physLev,data=brfss2013,Hess=TRUE) #Hessian used to get standard errors
(ctable<-coef(summary(brfss2_model)))
#calculating p_value by comparing the t-value against the stnd norm distr similar to a z-test
p<-pnorm(abs(ctable[, "t value"]),lower.tail = FALSE)*2
#combining p-value
(ctable<-cbind(ctable,"p value"=p))
#predicting probabilities that are easier to understand
plot(Effect(focal.predictors = c("incomeLev","bmiLev"),brfss2_model))
plot(Effect(focal.predictors = c("bmiLev","physLev"),brfss2_model))