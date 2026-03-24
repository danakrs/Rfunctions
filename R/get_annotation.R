#' Retrieve gene annotations from Ensembl or OrgDb
#'
#' Queries Ensembl via \pkg{biomaRt} to retrieve gene annotations for a vector
#' of gene identifiers. If the Ensembl server is unreachable, automatically
#' falls back to the appropriate \pkg{AnnotationDbi} OrgDb package.
#' The input ID type (Ensembl ID or gene symbol) is detected automatically from
#' the first element of \code{gene_vector}.
#'
#' @param gene_vector Character vector of gene identifiers. Either Ensembl gene
#'   IDs (e.g. \code{"ENSG00000000003"}) or gene symbols (HGNC for human,
#'   MGI for mouse).
#' @param organism Character string specifying the organism. One of
#'   \code{"human"} (default) or \code{"mouse"}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per element of
#'   \code{gene_vector} (preserving input order), and the following columns:
#'   \describe{
#'     \item{ENSEMBL}{Ensembl gene ID}
#'     \item{SYMBOL}{Gene symbol (HGNC for human, MGI for mouse)}
#'     \item{ENTREZID}{NCBI Entrez gene ID}
#'     \item{GENENAME}{Full gene name / description}
#'   }
#'   Genes with no annotation match are retained with \code{NA} in all
#'   annotation columns. Row names are set to the key column used for
#'   querying (\code{ENSEMBL} or \code{SYMBOL}). The attribute
#'   \code{"gene_input"} records the detected input type as an uppercase string
#'   (e.g. \code{"ENSEMBL_GENE_ID"}).
#'
#' @details
#' Input type detection is based on the first element of \code{gene_vector}:
#' identifiers starting with \code{"ENS"} (case-insensitive) are treated as
#' Ensembl IDs; all others are treated as gene symbols.
#'
#' When multiple annotation rows exist for a single gene, the row with a
#' non-\code{NA} \code{SYMBOL} is preferred; ties are broken by taking the
#' first row.
#'
#' The fallback OrgDb packages (\pkg{org.Hs.eg.db} or \pkg{org.Mm.eg.db})
#' must be installed but are only \emph{suggested} dependencies, as
#' \pkg{biomaRt} is tried first.
#'
#' @seealso
#' \code{\link[biomaRt]{getBM}}, \code{\link[AnnotationDbi]{select}}
#'
#' @importFrom AnnotationDbi select
#' @importFrom dplyr mutate coalesce across any_of select group_by arrange
#'   slice ungroup left_join sym all_of everything desc distinct
#' @importFrom tibble tibble column_to_rownames
#' @importFrom biomaRt useEnsembl getBM
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' # From Ensembl IDs
#' annot <- get_annotation(c("ENSG00000000003", "ENSG00000000005"),
#'                         organism = "human")
#'
#' # From gene symbols
#' annot <- get_annotation(c("EGFR", "KRAS"), organism = "human")
#'
#' # Use with DESeq2
#' annot <- get_annotation(rownames(dds), organism = "human")
#' SummarizedExperiment::rowData(dds) <- annot[rownames(dds), ]
#' }
#'
#' @export
get_annotation <- function(gene_vector,
                           organism = c("human", "mouse")) {
  organism <- match.arg(organism)

  # ---- Detect input ID type from gene_vector ----
  gene_input <- if (grepl("^ENS", gene_vector[1], ignore.case = TRUE)) {
    "ensembl_gene_id"
  } else if (organism == "human") {
    "hgnc_symbol"
  } else {
    "mgi_symbol"
  }
  message("Detected gene_input as: ", gene_input)

  # ---- Fetch annotations (biomaRt, with OrgDb fallback) ----
  mart_dataset <- switch(organism,
                         human = "hsapiens_gene_ensembl",
                         mouse = "mmusculus_gene_ensembl"
  )

  annot_df <- tryCatch({
    ensembl <- biomaRt::useEnsembl(
      biomart = "genes",
      dataset = mart_dataset,
      host    = "https://www.ensembl.org"
    )
    biomaRt::getBM(
      attributes = c("ensembl_gene_id", "hgnc_symbol", "entrezgene_id", "description"),
      filters    = gene_input,
      values     = gene_vector,
      mart       = ensembl
    )
  }, error = function(e) {
    message("biomaRt failed, falling back to OrgDb: ", conditionMessage(e))
    OrgDb <- if (organism == "human") {
      if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))
        stop("Package 'org.Hs.eg.db' is required. Install with: BiocManager::install('org.Hs.eg.db')")
      org.Hs.eg.db::org.Hs.eg.db
    } else {
      if (!requireNamespace("org.Mm.eg.db", quietly = TRUE))
        stop("Package 'org.Mm.eg.db' is required. Install with: BiocManager::install('org.Mm.eg.db')")
      org.Mm.eg.db::org.Mm.eg.db
    }
    keytype <- if (gene_input == "ensembl_gene_id") "ENSEMBL" else "SYMBOL"
    AnnotationDbi::select(
      OrgDb,
      keys    = gene_vector,
      keytype = keytype,
      columns = c("ENSEMBL", "SYMBOL", "ENTREZID", "GENENAME")
    )
  })

  # ---- Standardize column names across sources ----
  colnames(annot_df) <- toupper(colnames(annot_df))

  annot_df <- annot_df %>%
    dplyr::mutate(
      ENSEMBL  = dplyr::coalesce(
        dplyr::across(dplyr::any_of(c("ENSEMBL_GENE_ID", "ENSEMBL")))[[1]]
      ),
      SYMBOL   = dplyr::coalesce(
        dplyr::across(dplyr::any_of(c("HGNC_SYMBOL", "MGI_SYMBOL", "SYMBOL")))[[1]]
      ),
      ENTREZID = dplyr::coalesce(
        dplyr::across(dplyr::any_of(c("ENTREZGENE_ID", "ENTREZID")))[[1]]
      ),
      GENENAME = dplyr::coalesce(
        dplyr::across(dplyr::any_of(c("DESCRIPTION", "GENENAME")))[[1]]
      )
    ) %>%
    dplyr::select(ENSEMBL, SYMBOL, ENTREZID, GENENAME)

  # ---- Determine key column and deduplicate ----
  key_col <- switch(gene_input,
                    "ensembl_gene_id" = "ENSEMBL",
                    "hgnc_symbol"     = "SYMBOL",
                    "mgi_symbol"      = "SYMBOL"
  )
  key_sym <- dplyr::sym(key_col)

  annot_df <- annot_df %>%
    dplyr::group_by(!!key_sym) %>%
    dplyr::arrange(is.na(SYMBOL), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # ---- Left-join to preserve gene_vector order and include unmapped genes ----
  result <- tibble::tibble(!!key_sym := gene_vector) %>%
    dplyr::left_join(annot_df, by = key_col)

  message("Mapped: ", sum(!is.na(result[[key_col]])), " / ", length(gene_vector))
  rownames(result) <- result[[key_col]]

  attr(result, "gene_input") <- toupper(gene_input)
  return(result)

}
