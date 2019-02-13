# library(readr)
# library(ggplot2)
# library(caret)
# library(corrplot)

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

# prepare data.
dmy_ex_index <- dummyVars(" ~.", ex_prod_ready)
ex_prod_dummy <- data.frame(predict(dmy_ex_index, ex_prod_ready))

# remove features.
ex_prod_rmv <- ex_prod_dummy
ex_prod_rmv$ProductNum = NULL
ex_prod_rmv$BestSellersRank = NULL
ex_prod_rmv$ProductType.ExtendedWarranty = NULL


# correlation matrix.
cor_rmv <- cor(ex_prod_rmv)
cor_rmv
corrplot(cor_rmv)

colnames(ex_prod_rmv) <- c("PT_A", "PT_D", "PT_GC", "PT_L", 
                           "PT_N", "PT_PC", "PT_P", "PT_PS", 
                           "PT_SM", "PT_S", "PT_T", "Price", 
                           "5star", "4star", "3star", "2star",
                           "1star", "PosR", "NegR", "RecP", 
                           "ShipW", "ProD", "ProW", "ProH", 
                           "ProfM", "Vol")
