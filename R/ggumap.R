
ggumap <- function(.umap, label = NULL, .colour = label, title = "") {

  mapping.umap <- data.frame(
    id     = 1:NROW(.umap),
    dim1  = .umap[, 1],
    dim2  = .umap[, 2])

  ggp.umap <- mapping.umap %>%
    ggplot(aes(x = dim1, y = dim2), colour = .colour) +
    geom_point(alpha = 0.3, size = 0.2) +
    theme_bw() +
    guides(colour = FALSE) +
    labs(title = title)

  if(!is.null(label)){
    mapping.umap$label = as.factor(train.label)

    labels.cent <- mapping.umap %>%
      dplyr::group_by(label) %>%
      select(dim1, dim2) %>%
      summarize_all(mean)

    ggp.umap <- ggp.umap +
      ggrepel::geom_label_repel(data = labels.cent,
                                aes(label = label),
                                label.size = 0.1)
  }
  invisible(
    list(
      plot = ggp.umap,
      mapping = mapping.umap
    )
  )
}
