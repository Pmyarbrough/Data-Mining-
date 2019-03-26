# Case Study

#0.2 1. Load the dataset cpu.csv
cpu <- read_csv("https://www.iun.edu/~cisjw/ds/files/data/cpu.csv")
colnames(cpu) <- tolower(colnames(cpu))
summary(cpu)

#0.3.1 2. Shuffle rows
set.seed(52)
rows <- sample(nrow(cpu))
rows
cpu <- cpu[rows,]
cpu

#0.4 3. Subsetting the dataset
split <- round(nrow(cpu)*0.66)
train <- cpu[1:split, ]
test <- cpu[(split+1):nrow(cpu), ]

# 0.5 4. Fitting a linear regression model to the training set
model <- lm(performance ~ ., train) # dot for all regular attributes

# 0.6 5. View the model specifications
summary(model)

#Extract Coefficients
model$coefficients

#Read r-squared
summary(model)$r.squared

#0.7 6.Plot fitted values vs. true values in the training set
plot(model$fitted.values, train$performance, xlab="true_performance", ylab="fitted_performance", main="training set")

# 0.8 7. Calculate RMSE in the training set
error <- model$fitted.values - train$performance
rmse.train <- sqrt(mean(error^2))
rmse.train

# 0.9 8. Apply the model to the test set
scores <- predict(model, test)
scores

# 0.10 9. Calculate Error Metric: RMSE (root mean square error)
error <- scores - test$performance
rmse.test <- sqrt(mean(error^2))
rmse.test

# 0.11 10. Compare rmse.train and rmse.test
# rmse.train has the lower rmse value because we use the first 66% of the dataset.

############################################################################################
############################################################################################

# 0.12 Workout

# Loading datasets
library(tidyverse)
red <- read_delim(
  file = "https://www.iun.edu/~cisjw/I421/files/data/winequality-red.csv",
  delim = ";")
colnames(red)
nrow(red)
ncol(red)
summary(red)

white <- read_delim(
  file = "https://www.iun.edu/~cisjw/I421/files/data/winequality-white.csv",
  delim = ";")
colnames(white)
nrow(white)
ncol(white)
summary(white)


colnames(red) <- tolower(colnames(red))
colnames(white) <- tolower(colnames(white))
summary(red)
summary(white)

# Shuffle rows
set.seed(52)
rowsRed <- sample(nrow(red))
rowsWhite <- sample(nrow(white))
rowsRed
rowsWhite
red <- red[rowsRed, ]
white <- white[rowsWhite, ]
red
white

#0.4 3. Subsetting the dataset
splitR <- round(nrow(red)*0.66)
splitW <- round(nrow(white)*0.66)
trainR <- red[1:splitR, ]
trainW <- white[1:splitW, ]
testR <- red[(splitR+1):nrow(red), ]
testW <- white[(splitW+1):nrow(white), ]

# 0.5 4. Fitting a linear regression model to the training set
modelR <- lm(quality ~ ., trainR)
modelW <- lm(quality ~ ., trainW)
summary(modelR)
summary(modelW)
#Extract Coefficients
modelR$coefficients
modelW$coefficients
#Read r-squared
summary(modelR)$r.squared  #0.3400137
summary(modelW)$r.squared  #0.2980315

#0.7 6.Plot fitted values vs. true values in the training set
plot(modelR$fitted.values, trainR$quality, xlab="true_quality", ylab="fitted_quality", main="training set red wine")
plot(modelW$fitted.values, trainW$quality, xlab="true_quality", ylab="fitted_quality", main="training set white wine")

#0.8 7. Calculate RMSE in the training set
errorR <- modelR$fitted.values - trainR$quality
errorW <- modelW$fitted.values - trainW$quality

#Now calculate RMSE:
rmse.trainR <- sqrt(mean(errorR^2))
rmse.trainW <- sqrt(mean(errorW^2))
rmse.trainR #0.6431063
rmse.trainW #0.7357333

#0.9 8. Apply the model to the test set
scoresR <- predict(modelR, testR)
scoresR
scoresW <- predict(modelW, testW)
scoresW

#0.10 9. Calculate Error Metric: RMSE (root mean square error)
errorRR <- scoresR - testR$quality
errorWW <- scoresW - testW$quality

#Now calculate RMSE:
rmse.testR <- sqrt(mean(errorRR^2))
rmse.testW <- sqrt(mean(errorWW^2))
rmse.testR #0.6558411
rmse.testW #0.7828559



