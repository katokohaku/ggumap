require(tidyverse)
require(magrittr)

require(umap)
require(reticulate)
# py_install("pandas")
# py_install("scikit-learn")
# py_install("matplotlib")
# py_install("umap-learn")


reticulate::py_config()


set.seed(1)

getwd()
load("./data/mnist.rda")
# str(mnist, 0)
mnist.s <- mnist %>% sample_frac(0.3)
.data <- mnist.s[, -1]
labels <- mnist.s[, 1]

n <- NROW(.data)
.data %>% str(0)
labels %>% table()

umap.defaults
custom.config = umap.defaults
custom.config$min_dist = 0.01


system.time(
  mnist.umap <- .data %>%
    umap::umap(config = custom.config, method="umap-learn")
  # uwot::umap(metric = "cosine", n_neighbors=10, min_dist=0.001)
)
mnist.umap %>% str(2)
