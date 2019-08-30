require(tidyverse)
require(data.table)

#' @source \url{https://github.com/pjreddie/mnist-csv-png}

getwd()

curl.train <- "https://pjreddie.com/media/files/mnist_train.csv"
dest.train <- "./tools/mnist_train.csv"
download.file(curl.train, dest.train)
mnist.train <- fread("./tools/mnist_train.csv", data.table = FALSE)

curl.test <- "https://pjreddie.com/media/files/mnist_test.csv"
dest.test <- "./tools/mnist_test.csv"
download.file(curl.test, dest.test)
mnist.test <- fread("./tools/mnist_test.csv", data.table = FALSE)

mnist <- rbind(mnist.train, mnist.test)
usethis::use_data(mnist, overwrite = TRUE)
# save(mnist, file = "data/mnist.rda")

load("data/mnist.rda")
str(mnist, 0)

