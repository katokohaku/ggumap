install.packages("heatmaply", dependencies = TRUE)

library("heatmaply")

# https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html

x  <- as.matrix(datasets::mtcars)

# now let's spice up the dendrograms a bit:
library(dendextend)

row_dend  <- x %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 3) %>% set("branches_lwd", c(1,3)) %>%
  ladderize
#    rotate_DendSer(ser_weight = dist(x))
col_dend  <- x %>% t %>% dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k = 2) %>% set("branches_lwd", c(1,2)) %>%
  ladderize
#    rotate_DendSer(ser_weight = dist(t(x)))

heatmaply(percentize(x), Rowv = row_dend, Colv = col_dend,
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
            low = "blue", high = "red", midpoint = 0.5, limits = c(0, 1)))
