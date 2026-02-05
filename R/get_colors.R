
### ------------ get_colors.R---------------
require(RColorBrewer)
get_colors <- function () {

  if (!"RColorBrewer" %in% (.packages())) {suppressMessages(library(RColorBrewer))}

  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col_vector <- col_vector[-c(4, 7, 8, 12)] # remove certain colors
  return(col_vector)

}
