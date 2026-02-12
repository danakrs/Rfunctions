#' Save a ggplot as PNG and PDF
#'
#' @param filename Base filename (no extension)
#' @param plot A ggplot object
#' @param width Plot width (optional)
#' @param height Plot height (optional)
#' @importFrom ggplot2 ggsave
#' @export
savePlot <- function(
    filename,
    plot,
    width = NULL,
    height = NULL
) {

  # check that project globals exist
  if (!exists("plots_dir", inherits = TRUE))
    stop("plots_dir not defined in project environment")

  if (!exists("tag", inherits = TRUE))
    stop("tag not defined in project environment")

  out_dir <- file.path(plots_dir, tag)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  save_one <- function(ext) {
    ggplot2::ggsave(
      filename = file.path(out_dir, paste0(filename, ".", ext)),
      plot = plot,
      width = width,
      height = height
    )
  }

  save_one("png")
  save_one("pdf")
}
