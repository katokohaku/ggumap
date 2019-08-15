# tracing https://github.com/jdonaldson/rtsne/blob/master/tsne/R/tsne.R
# https://qiita.com/stfate/items/8988d01aad9596f9d586
# install.packages("tsne")
require(tsne)

eps = 2^(-52) # typical machine precision


X <- iris[,-5]
Y <- tsne::tsne(X, perplexity = 30)

# high dimensional conditional distribution: P(i|j)
P <- tsne:::.x2p(X, perplexity = 30)
str(P)
P <- P$P
diag(P)
round(P[1:10,1:10],2)

# high dimensional joint distribution: P(i,j)
P = .5 * (P + t(P))
diag(P)
round(P[1:10,1:10],4)


# low dimensional joint distribution: Q(i,j)
