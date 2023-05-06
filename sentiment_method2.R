

# load the files
test_txt <- readLines("test.txt")
train_txt <- readLines("train.txt")
val_txt <- readLines("val.txt")

# split each line in the test file by the semicolon separator
test_split <- strsplit(test_txt, ";")
train_split <- strsplit(train_txt, ";")
val_split <- strsplit(val_txt, ";")

# create a data frame with two columns: text and emotion
testv2 <- data.frame(
  text = sapply(test_split, "[[", 1),
  emotion = sapply(test_split, "[[", 2)
)

trainv2 <- data.frame(
  text = sapply(train_split, "[[", 1),
  emotion = sapply(train_split, "[[", 2)
)

valv2 <- data.frame(
  text = sapply(val_split, "[[", 1),
  emotion = sapply(val_split, "[[", 2)
)

# Combine the data
all_txt <- c(test_txt, train_txt, val_txt)

# Split the data by semicolon separator
all_split <- strsplit(all_txt, ";")

# Create a data frame with two columns: text and emotion
all_data <- data.frame(
  text = sapply(all_split, "[[", 1),
  emotion = sapply(all_split, "[[", 2)
)