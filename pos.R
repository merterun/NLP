library(udpipe)

# download and load the English model for UDPipe
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# POS tagging using UDPipe for each data frame
test_pos <- udpipe(test_lem$word, object = ud_model, tagger = "default")
train_pos <- udpipe(train_lem$word, object = ud_model, tagger = "default")
val_pos <- udpipe(val_lem$word, object = ud_model, tagger = "default")

head(test_pos)
