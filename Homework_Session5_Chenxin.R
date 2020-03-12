#Homework| Section 5 Support Vector Machines
# Chenxin

#-------------------------Exercise 1 Chap.9---------------------------------------------------

# 1. This problem involves hyperplanes in two dimensions.
#(a) Sketch the hyperplane 1 + 3X1 ??? X2 = 0. Indicate the set of points for which 1 + 3X1 ??? X2 > 0, as well as the set of points
# for which 1 + 3X1 ??? X2 < 0.
x1 = -10:10
x2 = 1 + 3 * x1
plot(x1, x2, type = "l", col = "red")


#(b) On the same plot, sketch the hyperplane ???2 + X1 + 2X2 = 0.Indicate the set of points for which ???2+ X1 +2X2 > 0, as well as
#the set of points for which ???2+ X1 + 2X2 < 0.

x1 = -10:10
x2 = (2 - x1)/2
plot(x1, x2, type = "l", col = "red")

#-------------------------Exercise 2 Chap.9---------------------------------------------------

#We have seen that in p = 2 dimensions, a linear decision boundary takes the form ??0+??1X1+??2X2 = 0.
# We now investigate a non-linear decision boundary.
# (a) Sketch the curve (1 + X1)^2 + (2 ??? X2)^2 = 4.
radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", 
     ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)

# (b) On your sketch, indicate the set of points for which (1 + X1)^2 + (2 ??? X2)^2 > 4, as well as the set of points for which
# (1 + X1)^2 + (2 ??? X2)^2 ??? 4.

radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", 
     ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

# (c) Suppose that a classifier assigns an observation to the blue class if (1 + X1)^2 + (2 ??? X2)^2 > 4, and to the red class otherwise. 
# To what class is the observation (0, 0) classified? (???1, 1)? (2, 2)? (3, 8)?
radius = 2
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE, inches = FALSE)

# (d) Argue that while the decision boundary in (c) is not linear in terms of X1 and X2, it is linear in terms of X1, X1^2,X2, and X2^2.
# The decision boundary is a sum of quadratic terms when expanded.
# (1+X1)^2+(2???X2)^2>4
# 1+2X1+X1^2+4???4X2+X2^2>4
# 5+2X1???4X2+X21+X2^2>4

#-------------------------Exercise 4 Chap.9---------------------------------------------------
#Generate a simulated two-class data set with 100 observations and two features in which there is a visible but non-linear separation 
# between the two classes. Show that in this setting, a support vector machine with a polynomial kernel (with degree greater than 1) 
# or a radial kernel will outperform a support vector classifier on the training data. 
# Which technique performs best on the test data? Make plots and report training and test error rates in order to back up your assertions.

set.seed(123)
x = rnorm(100)
y = 3 * x^2 + 4 + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 3
y[-train] = y[-train] - 3

plot(x[train], y[train], pch="+", lwd=4, col="red", ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="blue")

set.seed(234)
z = rep(0, 100)
z[train] = 1
# Take 25 observations each from train and -train
final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data.train = data.frame(x=x[final.train], y=y[final.train], z=as.factor(z[final.train]))
data.test = data.frame(x=x[-final.train], y=y[-final.train], z=as.factor(z[-final.train]))
library(e1071)
svm.linear = svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)

table(z[final.train], predict(svm.linear, data.train))
#    0  1
# 0 22  3
# 1  0 25

set.seed(1314)
svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)

table(z[final.train], predict(svm.poly, data.train))
#    0  1
# 0 21  4
# 1  0 25

set.seed(696)
svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, cost=10)
plot(svm.radial, data.train)

table(z[final.train], predict(svm.radial, data.train))
#    0  1
# 0 25  0
# 1  0 25
plot(svm.linear, data.test)

plot(svm.poly, data.test)

plot(svm.radial, data.test)

table(z[-final.train], predict(svm.linear, data.test))
#    0  1
# 0 19  6
# 1  0 25

table(z[-final.train], predict(svm.poly, data.test))
#    0  1
# 0 16  9
# 1  0 25

table(z[-final.train], predict(svm.radial, data.test))
#    0  1
# 0 22  3
# 1  0 25
#-------------------------Exercise 7 Chap.9---------------------------------------------------
# In this problem, you will use support vector approaches in order to predict whether a given car gets high or low gas mileage 
# based on the Auto data set.

library(ISLR)

# (a) Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage
# below the median.
gas.med = median(Auto$mpg)
new.var = ifelse(Auto$mpg > gas.med, 1, 0)
Auto$mpglevel = as.factor(new.var)

# (b) Fit a support vector classifier to the data with various values of cost, in order to predict whether a car gets high or 
# low gas mileage. 
# Report the cross-validation errors associated with different values of this parameter. Comment on your results.
library(e1071)
set.seed(2325)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01,0.1, 1, 5, 10, 100)))
summary(tune.out)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 1
# 
# - best performance: 0.007628205 
# 
# - Detailed performance results:
#   cost       error dispersion
# 1 1e-02 0.073846154 0.03469285
# 2 1e-01 0.040833333 0.03439580
# 3 1e+00 0.007628205 0.01228382
# 4 5e+00 0.012692308 0.01783081
# 5 1e+01 0.017756410 0.01700310
# 6 1e+02 0.030576923 0.02010376

# cross-validation error is minimized for cost=1.

# (c) Now repeat (b), this time using SVMs with radial and polynomial basis kernels, with different values of gamma 
# and degree and cost. Comment on your results.
set.seed(69)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.1,1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost degree
# 10      2
# 
# - best performance: 0.5585256 
# 
# - Detailed performance results:
#   cost degree     error dispersion
# 1   0.1      2 0.5737821 0.05417453
# 2   1.0      2 0.5737821 0.05417453
# 3   5.0      2 0.5737821 0.05417453
# 4  10.0      2 0.5585256 0.06011535
# 5   0.1      3 0.5737821 0.05417453
# 6   1.0      3 0.5737821 0.05417453
# 7   5.0      3 0.5737821 0.05417453
# 8  10.0      3 0.5737821 0.05417453
# 9   0.1      4 0.5737821 0.05417453
# 10  1.0      4 0.5737821 0.05417453
# 11  5.0      4 0.5737821 0.05417453
# 12 10.0      4 0.5737821 0.05417453

# The lowest cross-validation error is obtained for cost=10 and degree=2.

set.seed(996)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.1,1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 10  0.01
# 
# - best performance: 0.02551282 
# 
# - Detailed performance results:
#   cost gamma      error   dispersion
# 1   0.1 1e-02 0.08923077 0.0385241655
# 2   1.0 1e-02 0.07397436 0.0351145813
# 3   5.0 1e-02 0.04846154 0.0254752476
# 4  10.0 1e-02 0.02551282 0.0002702801
# 5   0.1 1e-01 0.07647436 0.0360725911
# 6   1.0 1e-01 0.05352564 0.0328605108
# 7   5.0 1e-01 0.03064103 0.0162640388
# 8  10.0 1e-01 0.03064103 0.0108819229
# 9   0.1 1e+00 0.55884615 0.0411953399
# 10  1.0 1e+00 0.06628205 0.0177007701
# 11  5.0 1e+00 0.06634615 0.0179417213
# 12 10.0 1e+00 0.06634615 0.0179417213
# 13  0.1 5e+00 0.55884615 0.0411953399
# 14  1.0 5e+00 0.50006410 0.0409459362
# 15  5.0 5e+00 0.50006410 0.0409459362
# 16 10.0 5e+00 0.50006410 0.0409459362
# 17  0.1 1e+01 0.55884615 0.0411953399
# 18  1.0 1e+01 0.52564103 0.0418716195
# 19  5.0 1e+01 0.52051282 0.0337579789
# 20 10.0 1e+01 0.52051282 0.0337579789
# 21  0.1 1e+02 0.55884615 0.0411953399
# 22  1.0 1e+02 0.55884615 0.0411953399
# 23  5.0 1e+02 0.55884615 0.0411953399
# 24 10.0 1e+02 0.55884615 0.0411953399

# for radial basis kernel, cost=10 and gamma=0.01.



# (d) Make some plots to back up your assertions in (b) and (c).
svm.linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
               degree = 2)
svm.radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)

plotpairs(svm.poly)

plotpairs(svm.radial)

#-------------------------Exercise 8 Chap.9---------------------------------------------------

#This problem involves the OJ data set which is part of the ISLR package.

library(ISLR)

# (a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.
set.seed(3366)
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

# (b) Fit a support vector classifier to the training data using cost=0.01, with Purchase as the response and the other variables
# as predictors. Use the summary() function to produce summary statistics, and describe the results obtained.
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm.linear)

# Call:
#   svm(formula = Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.01)
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.01 
# 
# Number of Support Vectors:  445
# 
# ( 221 224 )
# 
# Number of Classes:  2 
# 
# Levels: 
#   CH MM


# Support vector classifier creates 432 support vectors out of 800 training points. 
# Out of these, 217 belong to level CH and remaining 215 belong to level MM.



# (c) What are the training and test error rates?
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
# train.pred
#     CH  MM
# CH 442  51
# MM  81 226

(81 + 51)/(442 + 51 + 81 + 226) # 0.165

test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)

# test.pred
#     CH  MM
# CH 140  20
# MM  24  86

(24 + 20)/(140 + 20 + 24 + 86) # 0.162963


# (d) Use the tune() function to select an optimal cost. Consider values in the range 0.01 to 10.
set.seed(1314)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.03162278
# 
# - best performance: 0.17375 
# 
# - Detailed performance results:
#   cost   error dispersion
# 1   0.01000000 0.17750 0.03525699
# 2   0.01778279 0.17875 0.03775377
# 3   0.03162278 0.17375 0.03701070
# 4   0.05623413 0.17500 0.03908680
# 5   0.10000000 0.17625 0.04767147
# 6   0.17782794 0.17875 0.04678927
# 7   0.31622777 0.18000 0.04495368
# 8   0.56234133 0.18000 0.04830459
# 9   1.00000000 0.18000 0.05143766
# 10  1.77827941 0.17875 0.04966904
# 11  3.16227766 0.17625 0.04875178
# 12  5.62341325 0.17625 0.05415064
# 13 10.00000000 0.17625 0.05696307

# Tuning shows that optimal cost is 0.3162



# (e) Compute the training and test error rates using this new value for cost.
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
# train.pred
#     CH  MM
# CH 439  54
# MM  78 229

(78 + 54)/(439 + 54 + 78 + 229) # 0.165

test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
# test.pred
#     CH  MM
# CH 140  20
# MM  23  87

(23 + 20)/(140 + 20 + 23 + 87) # 0.1592593


# (f) Repeat parts (b) through (e) using a support vector machine with a radial kernel. Use the default value for gamma.
set.seed(410)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)

# Call:
#   svm(formula = Purchase ~ ., data = OJ.train, kernel = "radial")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# 
# Number of Support Vectors:  382
# 
# ( 191 191 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   CH MM


train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
# train.pred
#     CH  MM
# CH 454  39
# MM  84 223

(84 + 39)/(454 + 39 + 84 + 223) # 0.15375

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
# test.pred
#     CH  MM
# CH 145  15
# MM  31  79

(31 + 15)/(145 + 15 + 31 + 79) # 0.1703704

set.seed(755)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 0.5623413
# 
# - best performance: 0.1725 
# 
# - Detailed performance results:
#   cost   error dispersion
# 1   0.01000000 0.38375 0.05337563
# 2   0.01778279 0.38375 0.05337563
# 3   0.03162278 0.38375 0.05622685
# 4   0.05623413 0.21250 0.03173239
# 5   0.10000000 0.18750 0.03773077
# 6   0.17782794 0.19000 0.03106892
# 7   0.31622777 0.17750 0.03944053
# 8   0.56234133 0.17250 0.04887626
# 9   1.00000000 0.17750 0.04440971
# 10  1.77827941 0.18375 0.04084609
# 11  3.16227766 0.18875 0.03793727
# 12  5.62341325 0.18500 0.03216710
# 13 10.00000000 0.18625 0.03793727

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
# train.pred
#     CH  MM
# CH 452  41
# MM  85 222

(85 + 41)/(452 + 41 + 85 + 222) # 0.1575

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
# test.pred
#     CH  MM
# CH 144  16
# MM  31  79

(31 + 16)/(144 + 16 + 31 + 79) # 0.1740741

# (g) Repeat parts (b) through (e) using a support vector machine with a polynomial kernel. Set degree=2.

set.seed(7998)
svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)

# Call:
#   svm(formula = Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  1 
# degree:  2 
# coef.0:  0 
# 
# Number of Support Vectors:  453
# 
# ( 223 230 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   CH MM

train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred) # 0.1713

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred) # 0.1815

set.seed(322)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost
# 5.623413
# 
# - best performance: 0.18375 
# 
# - Detailed performance results:
#   cost   error dispersion
# 1   0.01000000 0.38250 0.05986095
# 2   0.01778279 0.36625 0.05653477
# 3   0.03162278 0.35875 0.06615691
# 4   0.05623413 0.35750 0.06101002
# 5   0.10000000 0.31750 0.05719120
# 6   0.17782794 0.23750 0.04208127
# 7   0.31622777 0.20750 0.03238227
# 8   0.56234133 0.20750 0.03782269
# 9   1.00000000 0.20500 0.04174992
# 10  1.77827941 0.19750 0.03987829
# 11  3.16227766 0.18625 0.04185375
# 12  5.62341325 0.18375 0.04126894
# 13 10.00000000 0.18750 0.03679900

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)   # 0.1512

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)  # 0.1741


# (h) Overall, which approach seems to give the best results on this data?
# Overall, radial basis kernel seems to be producing minimum misclassification error on both train and test data.

