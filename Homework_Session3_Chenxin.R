####################  Homework of Session 3 ####################

#-------------Chapter 5 excercise 3 --------------------------#

# a) Explain how k-fold cross-validation is implemented.

# First, randomly divid the set of observation into K groups with approximately equal size;
# Second, the first fold is treated as test set and the remaining K-1 folds are training set, fit the model to the trainning set and make prediction on the test set;
# Third, comput mean square error MSE;
# Fourth, the next fold is treated as test set and the rest are the trainging and compute MSE;
# Fifth, when every fold has been treated as test set and has computed K MSE, compute the K-fold CV estimate: (1/K)*(SUM of MSE)

# b) What are the advantages and disadvantages of k-fold crossvalidation relative to: i. The validation set approach?  ii. LOOCV?

# i: validation set approache would lead to high variance, and only a subset of the observations are used to fit the model, which may lead to overestimate the test error rate.

# ii: because LOOCV use as much as the number of observation to train the model, so that would result in less bias; but LOOCV has higher variance than K-fold CV because of the overlap bewteen the the training sets.
# And LOOCV are very time consuming. LOOCV will fit the model as many as the observation of the full data set times, and K-fold will only do K times which is set by choice.

#-------------Chapter 5 excercise 8 --------------------------#

####### a  ##########

# Generate a simulated data set as follows:
set.seed(1)
x = rnorm(100)
y = x-2*x^2+rnorm(100)

# In this data set, what is n and what is p? Write out the model used to generate the data in equation form.
# n = 100, p = 2.
# Y=X???2X^2+??

####### b  ##########

# Create a scatterplot of X against Y . Comment on what you find.
plot(x,y)
# it showed that there is a linear relationship between x and y. the points form a curve shap of line.

####### c  ##########

# Set a random seed, and then compute the LOOCV errors that result from fitting the following four models using least squares:
# Note you may find it helpful to use the data.frame() function to create a single data set containing both X and Y .
  
Data = data.frame(x, y)
set.seed(1)

library(boot)
# i
glm.fit <- glm(y ~ x)
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 7.288162 7.284744

# ii
glm.fit <- glm(y ~ poly(x, 2))
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 0.9374236 0.9371789

# iii
glm.fit <- glm(y ~ poly(x, 3))
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 0.9566218 0.9562538

# iv
glm.fit <- glm(y ~ poly(x, 4))
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 0.9539049 0.9534453

####### d  ##########

# Repeat (c) using another random seed, and report your results.Are your results the same as what you got in (c)? Why?
set.seed(8)

# i
glm.fit <- glm(y ~ x)
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 7.288162 7.284744 (same as c)

# ii
glm.fit <- glm(y ~ poly(x, 2))
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 0.9374236 0.9371789 (same as c)

# iii
glm.fit <- glm(y ~ poly(x, 3))
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 0.9566218 0.9562538 (same as c)

# iv
glm.fit <- glm(y ~ poly(x, 4))
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta
# 0.9539049 0.9534453 (same as c)

####### e  ##########

# Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.
# According to the error computed above, model (ii) has the smallest LOOCV error.
 

####### f  ##########

# Comment on the statistical significance of the coefficient estimates that results from fitting each of the models in (c) using least squares. Do these results agree with the conclusions drawn based on the cross-validation results?
  
summary(glm.fit)

# Call:
#   glm(formula = y ~ poly(x, 4))
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0550  -0.6212  -0.1567   0.5952   2.2267  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.55002    0.09591 -16.162  < 2e-16 ***
#   poly(x, 4)1   6.18883    0.95905   6.453 4.59e-09 ***
#   poly(x, 4)2 -23.94830    0.95905 -24.971  < 2e-16 ***
#   poly(x, 4)3   0.26411    0.95905   0.275    0.784    
# poly(x, 4)4   1.25710    0.95905   1.311    0.193    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 0.9197797)
# 
# Null deviance: 700.852  on 99  degrees of freedom
# Residual deviance:  87.379  on 95  degrees of freedom
# AIC: 282.3
# 
# Number of Fisher Scoring iterations: 2


# I don't understand very well after chapter 5.
# when I was doing this homework, I would look up on online solution:https://blog.princehonest.com/stat-learning/ and try to understand.
# But for some solution I still don't understand and I have commented them under the code.

#-------------Chapter 6 excercise 1 --------------------------#

# We perform best subset, forward stepwise, and backward stepwise selection on a single data set. 
# For each approach, we obtain p + 1 models, containing 0, 1, 2, . . . , p predictors. Explain your answers:

####### a  ##########

# Which of the three models with k predictors has the smallest training RSS?
# Best subset with k predictors has the smallest training RSS. 

####### b  ##########

# Which of the three models with k predictors has the smallest test RSS?
# solution:Best subset with k predictors has the smallest test RSS. 
# why?

####### c  ##########

# i. The predictors in the k-variable model identified by forward stepwise are a subset of the predictors in the (k+1)-variable model identified by forward stepwise selection.
# True

# ii. The predictors in the k-variable model identified by backward stepwise are a subset of the predictors in the (k + 1)-variable model identified by backward stepwise selection.
# True

# iii. The predictors in the k-variable model identified by backward stepwise are a subset of the predictors in the (k + 1)-variable model identified by forward stepwise selection.
# False

# iv. The predictors in the k-variable model identified by forward stepwise are a subset of the predictors in the (k+1)-variable model identified by backward stepwise selection.
# False

# v. The predictors in the k-variable model identified by best subset are a subset of the predictors in the (k + 1)-variable model identified by best subset selection.
# False


#-------------Chapter 6 excercise 9 --------------------------#
# In this exercise, we will predict the number of applications received using the other variables in the College data set.
library(ISLR)
names(College)
# [1] "Private"     "Apps"        "Accept"      "Enroll"      "Top10perc"   "Top25perc"   "F.Undergrad" "P.Undergrad" "Outstate"   
# [10] "Room.Board"  "Books"       "Personal"    "PhD"         "Terminal"    "S.F.Ratio"   "perc.alumni" "Expend"      "Grad.Rate" 

####### a  ##########

# Split the data set into a training set and a test set.

train.size <- nrow(College) / 2
train <- sample(1:nrow(College), train.size)
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]

####### b  ##########

# Fit a linear model using least squares on the training set, and report the test error obtained.

lm.fit <- lm(Apps~., data = College.train)
lm.pred <- predict(lm.fit, data = College.test)
mean((College.test[, "Apps"] - lm.pred)^2)
# test error: 27409540

####### c  ##########

# Fit a ridge regression model on the training set, with ?? chosen by cross-validation. 
# Report the test error obtained.

library(glmnet)
train.mat <- model.matrix(Apps~., data=College.train)
test.mat <- model.matrix(Apps~., data=College.test)

grid <- 10 ^ seq(4, -2, length=100)
ridge.mod <- cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid)
lambda.best <- ridge.mod$lambda.min
lambda.best # 7.054802

ridge.pred = predict(ridge.mod, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)
# test error: 1260183

####### d  ##########

# Fit a lasso model on the training set, with ?? chosen by crossvalidation. 
# Report the test error obtained, along with the number of non-zero coefficient estimates.

mod.lasso = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid)
lambda.best = mod.lasso$lambda.min
lambda.best  # 18.73817

lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)
# test error: 1259250

####### e  ##########

# Fit a PCR model on the training set, with M chosen by crossvalidation.
# Report the test error obtained, along with the value of M selected by cross-validation.

library(pls)
pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - pcr.pred)^2)
# test error: 2683340

####### f  ##########

# Fit a PLS model on the training set, with M chosen by crossvalidation.
# Report the test error obtained, along with the value of M selected by cross-validation.

pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - pls.pred)^2)
# test error: 1241500

####### g  ##########

# Comment on the results obtained. How accurately can we predict the number of college applications received?
# Is there much difference among the test errors resulting from these five approaches?

# The test errors are pretty large, espcially for the least squares.
# The test errors of ridge regression model and lasso model with ?? chosen by crossvalidation are very close and much smaller.


#-------------Chapter 7 excercise 9 --------------------------#

# This question uses the variables dis (the weighted mean of distances to five Boston employment centers) and nox (nitrogen oxides concentration in parts per 10 million) from the Boston data. 
# We will treat dis as the predictor and nox as the response.

####### a  ##########

# Use the poly() function to fit a cubic polynomial regression to predict nox using dis. 
# Report the regression output, and plot the resulting data and polynomial fits.

library(MASS)
attach(Boston)
set.seed(1)

lm.fit <- lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)

# Call:
#   lm(formula = nox ~ poly(dis, 3), data = Boston)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.121130 -0.040619 -0.009738  0.023385  0.194904 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.554695   0.002759 201.021  < 2e-16 ***
#   poly(dis, 3)1 -2.003096   0.062071 -32.271  < 2e-16 ***
#   poly(dis, 3)2  0.856330   0.062071  13.796  < 2e-16 ***
#   poly(dis, 3)3 -0.318049   0.062071  -5.124 4.27e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06207 on 502 degrees of freedom
# Multiple R-squared:  0.7148,	Adjusted R-squared:  0.7131 
# F-statistic: 419.3 on 3 and 502 DF,  p-value: < 2.2e-16

dislim <- range(dis)
dis.grid <- seq(from = dislim[1], to = dislim[2], by = 0.1)
lm.pred <- predict(lm.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, lm.pred, col = "red", lwd = 2)


####### b  ##########

# Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), 
# and report the associated residual sum of squares.

poly.rss <- rep(NA, 10)
for (i in 1:10) {
  lm.fit <- lm(nox ~ poly(dis, i), data = Boston)
  poly.rss[i] <- sum(lm.fit$residuals^2)
}
poly.rss
# 2.768563 2.035262 1.934107 1.932981 1.915290 1.878257 1.849484 1.835630 1.833331 1.832171


####### c  ##########

# Perform cross-validation or another approach to select the optimal degree for the polynomial, and explain your results.

library(boot)
all.deltas <- rep(NA, 10)
for (i in 1:10) {
  glm.fit <- glm(nox ~ poly(dis, i), data = Boston)
  all.deltas[i] <- cv.glm(Boston, glm.fit, K = 10)$delta[2]
}
plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)
# performed a 10-fold cv, the cv error decreased from 1 to 4, then increased. So 4 will be a better degree for the polynomial.


####### d  ##########

# Use the bs() function to fit a regression spline to predict nox using dis. 
# Report the output for the fit using four degrees of freedom. 
# How did you choose the knots? Plot the resulting fit.

library(splines)
sp.fit <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(sp.fit)

# Call:
#   lm(formula = nox ~ bs(dis, df = 4), data = Boston)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.124622 -0.039259 -0.008514  0.020850  0.193891 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       0.73447    0.01460  50.306  < 2e-16 ***
#   bs(dis, df = 4)1 -0.05810    0.02186  -2.658  0.00812 ** 
#   bs(dis, df = 4)2 -0.46356    0.02366 -19.596  < 2e-16 ***
#   bs(dis, df = 4)3 -0.19979    0.04311  -4.634 4.58e-06 ***
#   bs(dis, df = 4)4 -0.38881    0.04551  -8.544  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.06195 on 501 degrees of freedom
# Multiple R-squared:  0.7164,	Adjusted R-squared:  0.7142 
# F-statistic: 316.5 on 4 and 501 DF,  p-value: < 2.2e-16

sp.pred <- predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)


####### e  ##########

# Now fit a regression spline for a range of degrees of freedom, and plot the resulting fits 
# and report the resulting RSS. Describe the results obtained.

all.cv <- rep(NA, 16)
for (i in 3:16) {
  lm.fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = sum(lm.fit$residuals^2)
}
all.cv[-c(1, 2)]
# 1.934107 1.922775 1.840173 1.833966 1.829884 1.816995 1.825653 1.792535 1.796992 1.788999 1.782350 1.781838 1.782798 1.783546


####### f  ##########

# Perform cross-validation or another approach in order to select the best degrees of freedom for a regression spline on this data.
# Describe your results.

all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}

plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")
# the cv error is quite jumpy and has the smallest error at df12, so the best degree will be 12.



#-------------Chapter 7 excercise 10 --------------------------#

# This question relates to the College data set.

####### a  ##########

# Split the data into a training set and a test set. Using out-of-state tuition as the response and the other variables as the predictors,
# perform forward stepwise selection on the training set in order to identify a satisfactory model that uses just a subset of the predictors.

set.seed(1)
library(ISLR)
library(leaps)
attach(College)

names(College)
# [1] "Private"     "Apps"        "Accept"      "Enroll"      "Top10perc"   "Top25perc"   "F.Undergrad" "P.Undergrad" "Outstate"   
# [10] "Room.Board"  "Books"       "Personal"    "PhD"         "Terminal"    "S.F.Ratio"   "perc.alumni" "Expend"      "Grad.Rate" 

# split the data into a training set and a test set
train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]

# perform forward stepwise selection
reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
reg.summary = summary(reg.fit)

# set plot layout
par(mfrow = c(1, 3))
# plot cp with number of vars
# what is this cp?
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
# plot BIC with number of vars
# what is this BIC?
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
# plot adjusted R2 with number of vars
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi) 
# "(Intercept)" "PrivateYes"  "Room.Board"  "PhD"  "perc.alumni" "Expend"   "Grad.Rate"  

####### b  ##########

# Fit a GAM on the training data, using out-of-state tuition as the response and the features selected in the previous step as the predictors. 
# Plot the results, and explain your findings.
library(gam)
gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + 
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")


####### c  ##########

# Evaluate the model obtained on the test set, and explain the results obtained.
gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err
# gam.err:3349290

gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss
# test.rss: 0.7660016

####### d  ##########

# For which variables, if any, is there evidence of a non-linear relationship with the response?
summary(gam.fit)

# Call: gam(formula = Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, 
#                                                                    df = 2) + s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, 
#                                                                                                                             df = 2), data = College.train)
# Deviance Residuals:
#   Min       1Q   Median       3Q      Max 
# -7402.89 -1114.45   -12.67  1282.69  7470.60 
# 
# (Dispersion Parameter for gaussian family taken to be 3711182)
# 
# Null Deviance: 6989966760 on 387 degrees of freedom
# Residual Deviance: 1384271126 on 373 degrees of freedom
# AIC: 6987.021 
# 
# Number of Local Scoring Iterations: 2 
# 
# Anova for Parametric Effects
# Df     Sum Sq    Mean Sq F value    Pr(>F)    
# Private                  1 1778718277 1778718277 479.286 < 2.2e-16 ***
#   s(Room.Board, df = 2)    1 1577115244 1577115244 424.963 < 2.2e-16 ***
#   s(PhD, df = 2)           1  322431195  322431195  86.881 < 2.2e-16 ***
#   s(perc.alumni, df = 2)   1  336869281  336869281  90.771 < 2.2e-16 ***
#   s(Expend, df = 5)        1  530538753  530538753 142.957 < 2.2e-16 ***
#   s(Grad.Rate, df = 2)     1   86504998   86504998  23.309 2.016e-06 ***
#   Residuals              373 1384271126    3711182                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Anova for Nonparametric Effects
# Npar Df  Npar F     Pr(F)    
# (Intercept)                                         
# Private                                             
# s(Room.Board, df = 2)        1  1.9157    0.1672    
# s(PhD, df = 2)               1  0.9699    0.3253    
# s(perc.alumni, df = 2)       1  0.1859    0.6666    
# s(Expend, df = 5)            4 20.5075 2.665e-15 ***
#   s(Grad.Rate, df = 2)         1  0.5702    0.4506    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


