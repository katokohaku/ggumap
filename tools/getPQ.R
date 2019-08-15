# install.packages("tsne")
require(tsne)

eps = 2^(-52) # typical machine precision


X <- iris[,-5]
Y = tsne::tsne(X, perplexity = 30)
P <- tsne:::.x2p(X, perplexity = 30)
str(P)
P <- P$P
diag(P)
round(P[1:10,1:10],2)

P = .5 * (P + t(P))
diag(P)
round(P[1:10,1:10],4)
