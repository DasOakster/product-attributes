data.quality.reports <- function() {
      
      # Set up Working Directories, Source Scripts and Target files
      
      source.dir <- "D:/OneDrive/R Projects/product-attributes/DQ Functions/"
      dq.functions <- list.files(source.dir)
      lapply(paste(source.dir,dq.functions,sep = ""),source)
      

}