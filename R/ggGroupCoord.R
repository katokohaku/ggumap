
ggGroupCoord <- function(
  .coord,
  # title = "", xlab = "dim1", ylab = "dim2",
  ...,
  group.label = NULL,
  .colour = group.label,
  .dim1 = 1,
  .dim2 = 2,
  .alpha = 0.4,
  .size = 1
  ) {
  mapping.coord <- data.frame(
    id    = 1:NROW(.coord),
    dim1  = .coord[, .dim1],
    dim2  = .coord[, .dim2]
  )

  show_guide <- FALSE

  if (is.null(group.label)){
    if (!is.null(.colour)) {
      mapping.coord$label <- .colour
    } else {
      mapping.coord$label <- 0
    }
  } else {
    mapping.coord$label <- group.label
  }

  ggp.coord <- mapping.coord %>%
    ggplot(aes(x = dim1, y = dim2, colour = label)) +
    geom_point(alpha = .alpha, size = .size) +
    guides(colour = FALSE) +
    theme_bw() +
    labs(...)

  if (!is.null(group.label)) {

    labels.cent <- mapping.coord %>%
      dplyr::group_by(label) %>%
      select(dim1, dim2) %>%
      summarize_all(mean)

    ggp.coord <- ggp.coord +
      ggrepel::geom_label_repel(
        data = labels.cent,
        aes(label = label),
        label.size = 0.1)
  }

  return(ggp.coord)
}

require(uwot)
require(tidyverse)

# devtools::install_github("jlmelville/snedata")
# fashion <- snedata::download_fashion_mnist()
# save(mnist, file = "data/mnist.rda")
load("data/mnist.rda")

# res_sumap <- umap(fashion, y = fashion$Description, verbose = TRUE)
res_sumap <- umap(iris, y = iris$Species, verbose = TRUE)

# simple plot
ggGroupCoord(res_sumap)
# plot with color
ggGroupCoord(res_sumap, .colour = iris$Species, title = "with color", .size = 2)
# plot with group label
ggGroupCoord(res_sumap, group.label = iris$Species, title = "with group label")



ggp.umap


ggsave(fashion_sumap_ggp$plofilename, filename = "hoge.png")


fashion_train <- head(fashion, 60000)
fashion_test <- tail(fashion, 10000)

fashion_sumap_train <- umap(fashion_train, y = fashion_train$Description,
                            ret_model = TRUE, verbose = TRUE)

