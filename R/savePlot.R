### ------------ savePlot.R---------------
savePlot <- function(filename, p, width=NULL, height=NULL) {
  if (!is.null(width) && !is.null(height)) {
    ggsave(file.path(plots_dir, paste0("/",tag,"/",filename, ".png")), plot = p, width = width, height = height)
    #ggsave(file.path(plots_dir, paste0("/",prefix,"/",filename, ".svg")), plot = p, width = width, height = height)
    ggsave(file.path(plots_dir, paste0("/",tag,"/",filename, ".pdf")), plot = p, width = width, height = height)
  } else {
    ggsave(file.path(plots_dir, paste0("/",tag,"/",filename, ".png")), plot = p)
    #ggsave(file.path(plots_dir, paste0("/",prefix,"/",filename, ".svg")), plot = p)
    ggsave(file.path(plots_dir, paste0("/",tag,"/",filename, ".pdf")), plot = p)
  }
}
