data.quality.reports <- function() {
      
      # Read in Scripts with Data Quality Processing functions
      source.dir <- "D:/OneDrive/R Projects/wilko-data-quality/DQ Functions/"
      dq.functions <- list.files(source.dir)
      lapply(paste(source.dir,dq.functions,sep = ""),source)
      
      # Source Directory for the SAP extracts
      sap.dir <- ("D:/OneDrive/Work Files/Wilko/Data Cleanse/SAP Extracts")
      files.to.score <- list.files(sap.dir)
      
      # Target Directories for DQ Reporting Data Files
      report.dir <- "D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/"
      attribute.report.dir <- paste(report.dir,"Attribute Files/",sep = "")
      score.report.dir <- paste(report.dir,"Score Files/",sep = "")
      summary.report.dir <- paste(report.dir,"Summary Files/",sep = "")
      flag.report.dir <- paste(report.dir,"/DQ Flag Files/",sep = "")
      
      # Loop through files to score
      for(i in 1:NROW(files.to.score)) {
            
            # Identify PSA 1 and Set up CSF Directory
            psa1 <- substr(files.to.score[i],1,3)
            message(psa1)
            
            # Read in Data and Subset Columns
            psa1.site.data <- read.csv(paste(sap.dir,"/",files.to.score[i],sep = ""))
            psa1.site.data <- psa1.site.data[,c(1:4,6,10:21)]
            
            # Get correct Size regular expression for PSA 1
            check.size <- get.size(psa1)

            # Read in CSV files containing Crticial Attribute Web Types
            get.csf.files(psa1)
            
            # Create DQ columns
            psa1.site.data$size.format <- sapply(psa1.site.data$Size,grepl, pattern = check.size)
            psa1.site.data$size.required <- sapply(psa1.site.data$Type,function(x) is.element(x,size.required))
            
            return(psa1.site.data)
            
            } # End Loop

} # End Function