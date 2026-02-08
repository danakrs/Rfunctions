#' Extract DESeq2 results and normalized counts; optionally export to Excel
#'
#' @param dds A DESeqDataSet object
#' @param result DESeq2 result object (with stat column)
#' @param tag Character string used for naming output files
#' @param results_dir Directory where outputs are written
#' @param fdr_cutoff FDR/p-adj threshold for significance (default 0.05)
#' @param fc_cutoff Log2 fold change threshold for additional filtering
#' @return A list with DE results, normalized counts, and count tables
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column as_tibble
#' @export
extract_and_export <- function(dds, result, tag, results_dir, fdr_cutoff = 0.05, fc_cutoff = 0) {
  # ---- DE results tables ----
  res_tbl <- result %>%
    as.data.frame() %>%
    tibble::rownames_to_column("SYMBOL") %>%
    dplyr::arrange(.data$pvalue)

  res_sig <- res_tbl %>%
    dplyr::filter(.data$padj < fdr_cutoff)

  res_sig_fc <- res_sig %>%
    dplyr::filter(abs(.data$log2FoldChange) > fc_cutoff)

  # ---- normalized counts ----
  nc <- DESeq2::counts(dds, normalized = TRUE) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("SYMBOL")

  nc_sig <- nc %>%
    dplyr::filter(.data$SYMBOL %in% res_sig$SYMBOL)

  nc_sig_fc <- nc %>%
    dplyr::filter(.data$SYMBOL %in% res_sig_fc$SYMBOL)

  # ---- collect outputs ----
  de_res_list <- list(
    allGenes = res_tbl,
    sigGenes = res_sig,
    sigFCgenes = res_sig_fc
  )

  de_nc_list <- list(
    allGenes = nc,
    sigGenes = nc_sig,
    sigFCgenes = nc_sig_fc
  )

  # ---- export DE results to Excel ----
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(
    list(all_results = res_tbl),
    path = file.path(results_dir, paste0(tag, "_all_results.xlsx"))
  )

  # ---- export raw and normalized counts ----
  counts_dir <- file.path(results_dir, tag)
  dir.create(counts_dir, recursive = TRUE, showWarnings = FALSE)

  export_counts <- list(
    RawCounts = DESeq2::counts(dds, normalized = FALSE) %>%
      tibble::as_tibble(rownames = "SYMBOL"),
    NormalizedCounts = DESeq2::counts(dds, normalized = TRUE) %>%
      tibble::as_tibble(rownames = "SYMBOL")
  )

  writexl::write_xlsx(
    export_counts,
    file.path(counts_dir, "counts_lists.xlsx")
  )

  # ---- return structured list ----
  list(
    de_res = de_res_list,
    de_nc = de_nc_list,
    count_tables = export_counts
  )
}
