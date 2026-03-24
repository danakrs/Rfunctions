remove_duplicate_genes <- function(counts,
                                   annotation,
                                   gene_output       = "HGNC_SYMBOL",
                                   return_annotation = FALSE) {

  # ---- Recover gene_input set by get_annotation() ----
  gene_input <- attr(annotation, "gene_input")
  if (is.null(gene_input)) {
    stop("`annotation` must be produced by get_annotation() ",
         "or have a 'gene_input' attribute set manually.")
  }

  # ---- Compute gene means and join with annotation ----
  gene_means_df <- tibble(
    !!gene_input := rownames(counts),
    gene_means   = rowMeans(counts)
  )

  mapped_df <- annotation %>%
    left_join(gene_means_df, by = gene_input) %>%
    dplyr::select(all_of(gene_output), gene_means, everything())

  # ---- Keep highest-expressed row per output identifier ----
  filtered_mapped_df <- mapped_df %>%
    arrange(desc(gene_means)) %>%
    distinct(across(all_of(gene_output)), .keep_all = TRUE)

  # ---- Remap count matrix rownames to gene_output ----
  counts_filtered <- counts %>%
    as.data.frame() %>%
    rownames_to_column(var = gene_input) %>%
    left_join(
      filtered_mapped_df %>% dplyr::select(all_of(c(gene_input, gene_output))),
      by = gene_input
    ) %>%
    filter(!is.na(.data[[gene_output]])) %>%
    dplyr::select(-all_of(gene_input)) %>%
    column_to_rownames(var = gene_output) %>%
    as.matrix() %>%
    round()

  if (return_annotation) {
    return(list(counts = counts_filtered, annotation = filtered_mapped_df))
  } else {
    return(counts_filtered)
  }
}
