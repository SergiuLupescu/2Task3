library(readr)
library(ggplot2)
library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(dplyr)

# load dataset.
ex_product <- read.csv("existingproductattributes2017.2.csv")
new_product <- read.csv("newproductattributes2017.2.csv")

# explore dataset.
summary(ex_product)
summary(new_product)

# plot histogram.
ggplot(ex_product, aes(x = Volume)) + 
  geom_histogram(colour = "darkblue", fill = "lightblue")

# volume outliers and the row of the outliers.
volume_outlier <- boxplot.stats(ex_product$Volume)$out
volume_out_rows <- which(ex_product$Volume %in% volume_outlier)

# filter the outliers.
ex_prod_ready <- filter(ex_product, Volume < 7000)
new_prod_ready <- filter(new_product, Volume < 7000)
# filter duplicates.
ex_prod_ready <- filter(ex_prod_ready, ex_prod_ready$ProductType != "ExtendedWarranty")
new_prod_ready <- filter(new_prod_ready, new_prod_ready$ProductType != "ExtendedWarranty")
# prepare data.
dmy_ex_index <- dummyVars(" ~.", ex_prod_ready)
ex_prod_dummy <- data.frame(predict(dmy_ex_index, ex_prod_ready))
dmy_new_index <- dummyVars(" ~.", new_prod_ready)
new_prod_dummy <- data.frame(predict(dmy_new_index, new_prod_ready))


# remove features.
ex_prod_rmv <- ex_prod_dummy
ex_prod_rmv$ProductNum = NULL
ex_prod_rmv$BestSellersRank = NULL
ex_prod_rmv$ProductType.ExtendedWarranty = NULL
new_prod_rmv <- new_prod_dummy
new_prod_rmv$ProductNum = NULL
new_prod_rmv$BestSellersRank = NULL
new_prod_rmv$ProductType.ExtendedWarranty = NULL

# correlation matrix.
cor_rmv <- cor(ex_prod_rmv)
cor_rmv
corrplot(cor_rmv)

# product type as factor.
col_names <- c(colnames(ex_prod_rmv))
col_names_subset <- col_names[1:11]
for(i in col_names_subset) {
  ex_prod_rmv[ ,i] <- as.factor(ex_prod_rmv[ ,i])
}

for(i in c(1:11)) {
  new_prod_rmv[ ,i] <- as.factor(new_prod_rmv[ ,i])
}



colnames(ex_prod_rmv) <- c("PT_A", "PT_D", "PT_GC", "PT_L", 
                           "PT_N", "PT_PC", "PT_P", "PT_PS", 
                           "PT_SM", "PT_S", "PT_T", "Price", 
                           "x5star", "x4star", "x3star", "x2star",
                           "x1star", "PosR", "NegR", "RecP", 
                           "ShipW", "ProD", "ProW", "ProH", 
                           "ProfM", "Vol")

colnames(new_prod_rmv) <- c("PT_A", "PT_D", "PT_GC", "PT_L", 
                           "PT_N", "PT_PC", "PT_P", "PT_PS", 
                           "PT_SM", "PT_S", "PT_T", "Price", 
                           "x5star", "x4star", "x3star", "x2star",
                           "x1star", "PosR", "NegR", "RecP", 
                           "ShipW", "ProD", "ProW", "ProH", 
                           "ProfM", "Vol")
ex_prod_tree <- rpart(Vol ~ ., data = ex_prod_rmv)

# remove x5star.
ex_prod_rmv$x5star = NULL
new_prod_rmv$x5star = NULL

# decision tree and variable importance.
ex_prod_tree <- rpart(Vol ~ ., 
                      data = ex_prod_rmv, 
                      method = "anova", 
                      control = rpart.control(maxdepth = 5))
rpart.plot(ex_prod_tree)
varImp(ex_prod_tree)

# test and train set. 
set.seed(123)
test_index <- createDataPartition(ex_prod_rmv$Vol, p = .75, list = FALSE)
train_set <- ex_prod_rmv[test_index, ]
test_set <- ex_prod_rmv[-test_index, ]

# 10 fold cross validation.
train_ctrl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3)

# lm.
mod_lm <- train(Vol ~ x4star + PosR,
                data = ex_prod_rmv,
                method = "lm",
                trControl = train_ctrl)

# rf.
mod_rf <- train(Vol ~ x4star + PosR,
                data = ex_prod_rmv,
                method = "rf",
                trControl = train_ctrl)

# xgbt
mod_xgbt <- train(Vol ~ x4star + PosR,
                  data = ex_prod_rmv,
                  method = "xgbTree",
                  trControl = train_ctrl)

pred_rf <- predict(mod_rf, test_set)
pred_xgbt <-predict(mod_xgbt, test_set)

postResample(pred_rf, test_set$Vol)
postResample(pred_xgbt, test_set$Vol)

pred_new_rf <- predict(mod_rf, new_prod_rmv)
pred_new_rf
pred_new_xgbt <- predict(mod_xgbt, new_prod_rmv)
pred_new_xgbt
