#data
data <- read.table("data.csv",header = TRUE,sep = ",")

#Selecting few columns from it
new_data = data[,c(3,4,5,6,7,8,9,10,11)]

# correlation matrix
corr_mat <- cor(new_data)

#rounding it off to 2 decimal places
round(corr_mat,2)

#Enhanced correlation matrix
library(GGally)
ggpairs(new_data)

#summary of data (Descriptive stats)
summary(new_data)

#scatter plots wrt to each variable and their box plots too.
# --------------------------------------------------------
plot(growth_rate ~ literacyF,data = new_data)

#lm - linear model
growth_F <- lm(growth_rate ~ literacyF, data = new_data)

#abline used for plotting in same graph
abline(growth_F)
boxplot(new_data$literacyF,
        main = "Female Lit",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)


plot(growth_rate ~ sexratio,data = new_data)
growth_sex <- lm(growth_rate ~ sexratio, data = new_data)
abline(growth_sex)
boxplot(new_data$sexratio,
        main = "Sex Ratio",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

plot(growth_rate ~ percent_urban,data = new_data)
growth_urb <- lm(growth_rate ~ percent_urban, data = new_data)
abline(growth_urb)
boxplot(new_data$percent_urban,
        main = "Urban Population(%)",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

plot(growth_rate ~ percentST,data = new_data)
growth_st <- lm(growth_rate ~ percentST, data = new_data)
abline(growth_st)
boxplot(new_data$percentST,
        main = "ST Population (%)",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

# ---------------end of Scatter plots part -------------------------------

# Different models tried.
model1 <- lm(growth_rate ~ percent_urban + sexratio + percentSC + percentST + literacyF + literacyM, data=new_data)

#summary gives all the results(residuals, Fstat, t test, betas)
summary(model1) # show results

model2 <- lm(growth_rate ~ percent_urban + sexratio + percentSC + percentST + literacyF, data=new_data)
summary(model2) # show results

model3 <- lm(growth_rate ~ percent_urban + sexratio + percentSC + percentST, data=new_data)
summary(model3)

model4 <- lm(growth_rate ~ percent_urban + sexratio + percentSC + literacyF, data=new_data)
summary(model4)

model5 <- lm(growth_rate ~ literacyM + literacyF, data=new_data)
summary(model5)

# for the best model (right now - model 2), Heteroscedasticity rough check
# plot of residuals sqaure vs variable(one by one). If the graphs contained
# in a rectangular region. That's fine. Its better to remove Outliers, 
# it will help in better visualizing the plot

plot("^"(residuals(model2),2)~literacyF,data=new_data,ylim=c(0,500))
plot("^"(residuals(model2),2)~sexratio,data=new_data,ylim=c(0,1000),xlim = c(750,1150))
plot("^"(residuals(model2),2)~percent_urban,data=new_data,ylim=c(0,1000))
plot("^"(residuals(model2),2)~percentSC,data=new_data,ylim=c(0,1000))
plot("^"(residuals(model2),2)~percentST,data=new_data,ylim=c(0,1000))
plot("^"(residuals(model2),2)~fitted(model2),ylim=c(0,300))


library(lmtest)
#test for heteroscedasticity
#Breusch Pegan test
bptest(model2)

#White's test
bptest(model2, ~ literacyF*sexratio + literacyF*percent_urban + literacyF*percentSC + literacyF*percentST+ sexratio*percent_urban + sexratio*percentSC + sexratio*percentST + percent_urban*percentSC + percent_urban*percentST + percentSC*percentST + I(literacyF^2) + I(sexratio^2) + I(percent_urban^2)  + I(percentSC^2) + I(percentST^2),data = new_data[,c(2,3,4,5,7,9)])

#no correlation between residuals and regressors
cor(residuals(model2),new_data[,c(2,3,4,5,7,9)])

#no correlation between residuals and regressors
cor(residuals(model2),fitted(model2))

# Multicollinearity check
#if vif value greater than 10 in any model. Thats where problem arises. Otherwise ok.
library(HH)
vif(model2)
vif(model1)
vif(model3)
vif(model4)
vif(model5)