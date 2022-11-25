library(tidyverse)
library(caret)
library(rpart)
library(pROC)

train <- read.csv("train.csv", TRUE, ",")
View(train)

# boxplots and removing outliers

summary(train)
ds0 <- train %>% filter(train$price_range == 0)
ds1 <- train %>% filter(train$price_range == 1)
ds2 <- train %>% filter(train$price_range == 2)
ds3 <- train %>% filter(train$price_range == 3)
pdf(file = "boxplots.pdf")
for (i in 3:ncol(ds0)) {
    par(mfrow = c(1, 2))
    boxplot(ds0[, i], col = "blue", main = colnames(ds0[i]))
    ds0 <- ds0[!ds0[, i] %in% boxplot.stats(ds0[, i])$out, ]
    boxplot(ds0[, i], col = "red", main = colnames(ds0[i]))
}
for (i in 3:ncol(ds1)) {
    par(mfrow = c(1, 2))
    boxplot(ds1[, i], col = "blue", main = colnames(ds1[i]))
    ds1 <- ds1[!ds1[, i] %in% boxplot.stats(ds1[, i])$out, ]
    boxplot(ds1[, i], col = "red", main = colnames(ds1[i]))
}
for (i in 3:ncol(ds2)) {
    par(mfrow = c(1, 2))
    boxplot(ds2[, i], col = "blue", main = colnames(ds2[i]))
    ds2 <- ds2[!ds2[, i] %in% boxplot.stats(ds2[, i])$out, ]
    boxplot(ds2[, i], col = "red", main = colnames(ds2[i]))
}
for (i in 3:ncol(ds3)) {
    par(mfrow = c(1, 2))
    boxplot(ds3[, i], col = "blue", main = colnames(ds3[i]))
    ds3 <- ds3[!ds3[, i] %in% boxplot.stats(ds3[, i])$out, ]
    boxplot(ds3[, i], col = "red", main = colnames(ds3[i]))
}

dev.off()
# combine the datasets
train <- rbind(ds0, ds1, ds2, ds3)
view(train)

# run getwd() to get the current working directry and open that and
# find a pdf with name boxplots.pdf which contains boxplots
# of all attributes with/without outliers.

# removing na values

train <- na.omit(train)
View(train)

# correlation analysis

library(dplyr)
a <- train %>%
    select(c(battery_power:wifi))
View(a)
a1 <- subset(a, select = -c(three_g))
b <- as.data.frame(cor(a1))
View(b)

# removing redundant features
for (i in seq_len(nrow(b))){
    for (j in seq_len(nrow(b))){
        if (b[i, j] > 0.8 && b[i, j] != 1) {
            for (k in seq_len(nrow(train))) {
                if (identical(colnames(train)[k], colnames(b)[j])) {
                    train <- train[-k]
                }
            }
        }
    }
}

#checking correlation with target
a <- train %>%
    select(c(battery_power:price_range))
View(a)
a1 <- subset(a, select = -c(three_g))
b <- as.data.frame(cor(a1))
View(b)

View(train)
train <- filter(train)
View(train)

# min max normalization

# x1 <- min(train$battery_power)
# y1 <- max(train$battery_power)

# for (l in 1:length(train$battery_power)) {
#     z1 <- train$battery_power[l]
#     train$battery_power[l] <- (z1 - x1) / (y1 - x1) * (1200 - 600) + 600
# }


# x2 <- min(train$px_height)
# y2 <- max(train$px_height)

# for (l in 1:length(train$px_height)) {
#     z2 <- train$px_height[l]
#     train$px_height[l] <- (z2 - x2) / (y2 - x2) * (1000 - 500) + 500
# }

# x3 <- min(train$px_width)
# y3 <- max(train$px_width)

# for (l in 1:length(train$px_width)) {
#     z3 <- train$px_width[l]
#     train$px_width[l] <- (z3 - x3) / (y3 - x3) * (1000 - 500) + 500
# }

# View(train)

training_samples <- train$price_range %>%
    createDataPartition(p = 0.66, list = FALSE)
train_data <- train[training_samples, ]
test_data <- train[-training_samples, ]

model <- rpart(price_range ~ ., data = train_data, method = "class")

par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 2)
print(model, digits = 2)

predict_price <- model %>% predict(test_data, "class")
print(predict_price)

print(paste0(
    "    Accuracy: ",
    mean(predict_price == test_data$price_range) * 100, "%"
))
confmat <- table(predict_price, as.factor(test_data$price_range))
print(confmat)

precision0 <- confmat[1, 1] /
    (confmat[1, 1] + confmat[2, 1] + confmat[3, 1] + confmat[4, 1])
precision1 <- confmat[2, 2] /
    (confmat[1, 2] + confmat[2, 2] + confmat[3, 2] + confmat[4, 2])
precision2 <- confmat[3, 3] /
    (confmat[1, 3] + confmat[2, 3] + confmat[3, 3] + confmat[4, 3])
precision3 <- confmat[4, 4] /
    (confmat[1, 4] + confmat[2, 4] + confmat[3, 4] + confmat[4, 4])
print(paste0(
    "    Precision: ", precision0, ", ", precision1, ", ",
    precision2, ", ", precision3
))

recall0 <- confmat[1, 1] /
    (confmat[1, 1] + confmat[1, 2] + confmat[1, 3] + confmat[1, 4])
recall1 <- confmat[2, 2] /
    (confmat[2, 1] + confmat[2, 2] + confmat[2, 3] + confmat[2, 4])
recall2 <- confmat[3, 3] /
    (confmat[3, 1] + confmat[3, 2] + confmat[3, 3] + confmat[3, 4])
recall3 <- confmat[4, 4] /
    (confmat[4, 1] + confmat[4, 2] + confmat[4, 3] + confmat[4, 4])
print(paste0(
    "    Recall: ", recall0, ", ", recall1, ", ", recall2, ", ",
    recall3
))

f1score0 <- 2 * precision0 * recall0 / (precision0 + recall0)
f1score1 <- 2 * precision1 * recall1 / (precision1 + recall1)
f1score2 <- 2 * precision2 * recall2 / (precision2 + recall2)
f1score3 <- 2 * precision3 * recall3 / (precision3 + recall3)
print(paste0(
    "    F1 Score: ", f1score0, ", ", f1score1, ", ", f1score2, ", ",
    f1score3
))

pred <- predict(model, test_data, type = "prob")
pred <- pred[, 2]
roc <- roc(test_data$price_range, pred, plot = TRUE, print.auc = TRUE)
auc <- auc(roc)
print(paste0("    AUC: ", auc))
plot(roc,
    print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
    max.auc.polygon = TRUE, print.thres = "best",
    print.thres.best.method = "closest.topleft", main = "ROC Curve"
)
