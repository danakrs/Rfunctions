#' Plot top enriched GSEA pathways
#'
#' @param fgsea_res Data.frame output of fgsea
#' @param title Plot title
#' @param n_show Number of pathways to display
#' @return ggplot2 object
#' @importFrom magrittr %>%
#' @importFrom dplyr filter slice_max mutate arrange
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 ggplot aes geom_col coord_flip scale_fill_manual theme_minimal labs
#' @export
plot_gsea_bar <- function(fgsea_res, title, n_show = 20) {
  plot_df <- fgsea_res %>%
    dplyr::filter(!is.na(.data$padj), .data$padj < 0.05) %>%
    dplyr::slice_max(order_by = abs(.data$NES), n = n_show) %>%
    dplyr::mutate(
      direction = .data$NES > 0,
      pathway_clean = stringr::str_replace_all(.data$pathway, "_", " ")
    ) %>%
    dplyr::arrange(.data$NES) %>%
    dplyr::mutate(pathway_clean = factor(.data$pathway_clean, levels = .data$pathway_clean))

  ggplot2::ggplot(plot_df, ggplot2::aes(.data$pathway_clean, .data$NES, fill = .data$direction)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = "darkblue", "TRUE" = "firebrick"),
      labels = c("Down", "Up")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = "Pathway",
      y = "Normalized Enrichment Score",
      fill = "Direction"
    )
}
