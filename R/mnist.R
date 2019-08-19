#' Conversion for the MNIST dataset to table format
#'
#' 10,000 examples of 28x28 image from mnist test set.
#'
#' @format A data frame with 10000 rows of 785 (= 1 + 28*28) variables:
#' \describe{
#'   \item{first colmn}{ label for handwritten digits (0-9). }
#'   \item{folloing 2nd-785th colmn}{ Pixel values (0-255). 0 means background (white), 255 means foreground (black). }
#'
#' }
#' @source \url{https://github.com/pjreddie/mnist-csv-png}
"mnist"
