library(readr)
library(ggplot2)
library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(reshape)

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
#ex_prod_rmv <- ex_prod_dummy
#ex_prod_rmv$ProductNum = NULL
#ex_prod_rmv$BestSellersRank = NULL
#ex_prod_rmv$ProductType.ExtendedWarranty = NULL
#new_prod_rmv <- new_prod_dummy
#new_prod_rmv$ProductNum = NULL
#new_prod_rmv$BestSellersRank = NULL
#new_prod_rmv$ProductType.ExtendedWarranty = NULL



# product type as factor.
#col_names <- c(colnames(ex_prod_rmv))
#col_names_subset <- col_names[1:11]
#for(i in col_names_subset) {
#  ex_prod_rmv[ ,i] <- as.factor(ex_prod_rmv[ ,i])
#}

#for(i in c(1:11)) {
#  new_prod_rmv[ ,i] <- as.factor(new_prod_rmv[ ,i])
#}



colnames(ex_prod_dummy) <- c("PT_Acc", "PT_Dis", "PT_ExWa", "PT_GaCo", 
                             "PT_Lap", "PT_Net", "PT_PC", "PT_Pri", 
                             "PT_PriSup", "PT_SmaP", "PT_Sof", "PT_Tab", 
                             "ProdNu", "Price", "x5star", "x4star", 
                             "x3star", "x2star", "x1star", "PosRew", 
                             "NegRew", "RecPro", "BSR", "ShipW", "ProDep", 
                             "ProWei", "ProHei", "ProfM", "Vol")

colnames(new_prod_dummy) <- c("PT_Acc", "PT_Dis", "PT_ExWa", "PT_GaCo", 
                              "PT_Lap", "PT_Net", "PT_PC", "PT_Pri", 
                              "PT_PriSup", "PT_SmaP", "PT_Sof", "PT_Tab", 
                              "ProdNu", "Price", "x5star", "x4star", 
                              "x3star", "x2star", "x1star", "PosRew", 
                              "NegRew", "RecPro", "BSR", "ShipW", "ProDep", 
                              "ProWei", "ProHei", "ProfM", "Vol")
# correlation matrix.
cor_mat <- cor(ex_prod_dummy %>% select(-ProdNu, 
                                        -BSR, 
                                        -PT_ExWa))

corrplot(cor_mat, method = "square")

ex_prod_tree <- rpart(Vol ~ ., data = ex_prod_dummy)
rpart.plot(ex_prod_tree)
# remove x5star.
ex_prod_dummy <- ex_prod_dummy %>% select(-x5star)
new_prod_dummy <- new_prod_dummy %>%  select(-x5star)

# decision tree and variable importance.
ex_prod_tree <- rpart(Vol ~ ., 
                      data = ex_prod_dummy, 
                      method = "anova", 
                      control = rpart.control(maxdepth = 5))
rpart.plot(ex_prod_tree)
varImp(ex_prod_tree)

# test and train set. 
set.seed(123)
test_index <- createDataPartition(ex_prod_dummy$Vol, p = .75, list = FALSE)
train_set <- ex_prod_dummy[test_index, ]
test_set <- ex_prod_dummy[-test_index, ]

# 10 fold cross validation.
train_ctrl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3)

# modelling.
comp_mod <- c()
for(i in c("lm", "rf", "xgbTree")) {
  mod <- train(Vol ~ x4star + PosRew,
               data = ex_prod_dummy,
               method = i,
               trControl = train_ctrl)
  pred <- predict(mod, test_set)
  pred_met <- postResample(pred, test_set$Vol)
  comp_mod <- cbind(comp_mod, pred_met)
}

colnames(comp_mod) <- c("lm", "rf", "xgbTree")
plot_comp_mod <- melt(data = comp_mod)
colnames(plot_comp_mod) <- c("ErrorMetric", "Model", "Value")
ggplot(plot_comp_mod, aes(x = Model, y = Value, fill = ErrorMetric)) +
  geom_col() +
  facet_grid(ErrorMetric ~ ., scales = "free")



pred_new_xgbt <- predict(mod_xgbt, new_prod_dummy)
pred_new_xgbt
pred_new_rf <- predict(mod_rf, new_prod_dummy)
pred_new_rf

write.xlsx(pred_new_rf, file = "predictions.xlsx")

colnames(ex_product) <- c("PT", "ProdNu", "Price", "x5star", "x4star", 
                          "x3star", "x2star", "x1star", "Pos", 
                          "Neg", "RecPro", "BSR", "ShipW", "ProDep", 
                          "ProWei", "ProHei", "ProfM", "Vol")

corrplot.mixed(cor(ex_product %>% select(x5star, x4star, 
                                   x3star, x2star, 
                                   x1star, Pos, 
                                   Neg, Vol)),
               upper = "square", number.cex = 1)
