# load the files
test_txt <- readLines("test.txt")
train_txt <- readLines("train.txt")
val_txt <- readLines("val.txt")

# split each line in the test file by the semicolon separator
test_split <- strsplit(test_txt, ";")
train_split <- strsplit(train_txt, ";")
val_split <- strsplit(val_txt, ";")

# create a data frame with two columns: text and emotion
test <- data.frame(
  text = sapply(test_split, "[[", 1),
  emotion = sapply(test_split, "[[", 2)
)

train <- data.frame(
  text = sapply(train_split, "[[", 1),
  emotion = sapply(train_split, "[[", 2)
)

val <- data.frame(
  text = sapply(val_split, "[[", 1),
  emotion = sapply(val_split, "[[", 2)
)

# Check out the data frames
head(val)
head(test)
head(train)

library(tidyverse)

