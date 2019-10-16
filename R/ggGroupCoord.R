
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

