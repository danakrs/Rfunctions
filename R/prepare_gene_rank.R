#' Prepare a named vector of gene ranks for GSEA
#'
#' @param res DESeq2 result object with stat column
#' @return Named numeric vector: names = genes, values = ranking statistic
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr filter arrange
#' @importFrom tibble rownames_to_column
#' @importFrom stats setNames
#' @export
prepare_gene_rank <- function(res) {
  df <- res %>%
    as.data.frame() %>%
    tibble::rownames_to_column("gene") %>%
    dplyr::filter(!is.na(.data$stat)) %>%
    dplyr::arrange(dplyr::desc(.data$stat))

  stats::setNames(df$stat, df$gene)
}

