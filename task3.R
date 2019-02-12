# library(readr)
# library(ggplot2)
# library(caret)

# load dataset.
ex_product <- read.csv("existingproductattributes2017.2.csv")
new_product <- read.csv("newproductattributes2017.2.csv")

# explore dataset.
summary(ex_product)
summary(new_product)

# prepare data.
dmy_existing <- dummyVars(" ~.", ex_product)
ready_ex_product <- data.frame(predict(dmy_existing, ex_product))

