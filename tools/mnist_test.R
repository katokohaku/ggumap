require(tidyverse)
require(data.table)

# mnist <- fread("tools/mnist_test.csv", data.table = FALSE)
# usethis::use_data(mnist)
# save(mnist, file = "data/mnist.rda")

load("data/mnist.rda")
str(mnist, 0)
df.u <- mnist[, -1]

p = 30
n = nrow(df.u)

system.time(
  ri <- Rtsne::Rtsne(unique(df.u), perplexity = p)
)
kld <- last(ri$itercosts)
tmp <- c(
  perplexity = p,
  KLdist     = kld,
  pen.KLdist = kld + log(n) * p/n)

# KL <- rbind(KL, tmp)
cat(tmp, "\n")
plot(ri$Y[,1], ri$Y[,2])
