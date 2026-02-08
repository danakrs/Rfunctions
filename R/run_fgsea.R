#' Run fgsea enrichment
#'
#' @param gene_rank Named numeric vector
#' @param pathways List of gene sets
#' @param minSize Minimum gene set size
#' @param maxSize Maximum gene set size
#' @param nperm Number of permutations
#' @return fgsea result data.frame
#' @importFrom magrittr %>%
#' @export
run_fgsea <- function(gene_rank, pathways, minSize = 10, maxSize = 500, nperm = 10000) {
  fgsea::fgsea(
    pathways = pathways,
    stats = gene_rank,
    minSize = minSize,
    maxSize = maxSize,
    nperm = nperm
  ) %>%
    as.data.frame()
}
