library(tidyverse)
churn <- read_csv("https://www.iun.edu/~cisjw/ds/files/data/churn_customer.csv")
summary(churn)
churn %>% slice(1:5)
churn <- churn %>% mutate(churn_label = ifelse(ChurnIndicator > 0.5, 1, 0))
churn %>% ggplot(aes(x = ChurnIndicator, fill = as.factor(churn_label))) +
  geom_bar()+
  scale_y_log10()+
  labs(y = "log10(count)")
write_csv(churn,
          file.path("churn_v2.csv"),
          quote_escape = "double")
churn %>% group_by(churn_label) %>% summarise(count = n())




#4.1 Task 1 Churn into rank and diagram

library(polycor)
churn.cor <- hetcor(churn$ChurnIndicator, churn$churn_label, churn$Technology, churn$Age, churn$AverageBill,
                    churn$SupportCallsLastYear)
churn.cor$correlations
names <- c("Indicator", "Label", "Technology", "Age", "AverageBill", "SupportCallsLastYear")
churn.corrmat <- as_tibble(churn.cor$correlations)
colnames(churn.corrmat) <- names

churn.corrmat

churn.corrmat <- churn.corrmat %>%
  mutate(attribute=names) %>%
  select(attribute, Indicator, Label, Technology, Age, AverageBill, SupportCallsLastYear)

rank <- churn.corrmat %>%
  select(attribute, Indicator) %>%
  mutate(squared.correlation = Indicator^2) %>%
  arrange(desc(squared.correlation))

rank
rank$attribute

install.packages("corrplot")
library(corrplot)

churn.cor <- hetcor(churn$ChurnIndicator, churn$churn_label, churn$Technology, churn$Age, churn$AverageBill,
                    churn$SupportCallsLastYear)

corrplot(churn.cor$correlations, method="shade", type="upper", tl.col="black", tl.srt=45)


# Linear Regression
library(tidyverse)
churn <- read_csv("https://www.iun.edu/~cisjw/ds/files/data/churn_customer.csv")
colnames(churn) <- tolower(colnames(churn))
summary(churn)

set.seed(52)
rowsChurn <- sample(nrow(churn))
rowsChurn
churn <- churn[rowsChurn, ]
churn

splitChurn <- round(nrow(churn)*0.66)
trainChurn <- churn[1:splitChurn, ]
testChurn <- churn[(splitChurn+1):nrow(churn), ]

modelChurn <- lm(churnindicator ~ ., trainChurn)
summary(modelChurn)

modelChurn$coefficients
summary(modelChurn)$r.squared #0.5215162

plot(modelChurn$fitted.values, trainChurn$churnindicator, xlab="true_churnindicator", ylab="fitted_churnindicator", main="churn plot")

errorChurn <- modelChurn$fitted.values - trainChurn$churnindicator
rmse.trainChurn <- sqrt(mean(errorChurn^2))
rmse.trainChurn #0.03770924

scoresChurn <- predict(modelChurn, testChurn)
scoresChurn

errorChurn <- scoresChurn - testChurn$churnindicator
rmse.testChurn <- sqrt(mean(errorChurn^2))
rmse.testChurn #0.03940518



# Case Study

cpu <- read_csv("https://www.iun.edu/~cisjw/ds/files/data/cpu.csv")
colnames(cpu) <- tolower(colnames(cpu))
summary(cpu)

set.seed(52)
rows <- sample(nrow(cpu))
rows
cpu <- cpu[rows,]
cpu

split <- round(nrow(cpu)*0.66)
train <- cpu[1:split, ]
test <- cpu[(split+1):nrow(cpu), ]


model <-lm(performance ~ myct+mmin+mmax+cach+chmin+chmax, train)
