#' Save a ggplot as PNG and PDF
#'
#' @param filename Base filename (no extension)
#' @param plot A ggplot object
#' @param tag Subfolder name
#' @param width Plot width (optional)
#' @param height Plot height (optional)
#' @importFrom ggplot2 ggsave
#' @export
savePlot <- function(
    filename,
    plot,
    tag = tag,
    plots_dir = plots_dir,
    width = NULL,
    height = NULL
) {

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



