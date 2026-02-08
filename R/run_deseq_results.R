#' Run DESeq2 differential expression with Wald test and ashr shrinkage
#'
#' @param dds A DESeqDataSet object
#' @param contrast Character vector of length 3 specifying contrast (e.g. c("group", "SOS", "WT"))
#' @return A DESeqResults object with LFC shrinkage and preserved Wald statistic
#' @export
run_deseq_results <- function(dds, contrast) {
  # Check if IHW package is available
  if (!requireNamespace("IHW", quietly = TRUE)) {
    stop("Package 'IHW' is required but not installed. Please install it.")
  }

  # Unshrunken DESeq2 results with IHW
  res_unshrunk <- DESeq2::results(
    dds,
    contrast = contrast,
    test = "Wald",
    filterFun = IHW::ihw
  )

  # LFC shrinkage using ashr
  res_shr <- DESeq2::lfcShrink(
    dds,
    contrast = contrast,
    type = "ashr",
    res = res_unshrunk
  )

  # Preserve Wald statistic explicitly for downstream analyses
  res_shr$stat <- res_unshrunk$stat

  # Store contrast label as attribute
  attr(res_shr, "contrast_label") <- paste(contrast, collapse = "_")

  res_shr
}
