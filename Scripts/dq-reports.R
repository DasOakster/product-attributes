data.quality.reports <- function() {
      
      # This function is designed to take a data extract from either SAP or Wilko.com and run the Data Quality checks on the Attribute fields
      # These checks include: Format, Completeness and Consistency
      # Because different DQ rules are applied to each PSA1 the process creates PSA level files and then combines them into 1
      # Scores out of 100 are allocated to each product
      # Scored files are then exported for use in Excel Data Quality reports
      
      
      # Read in Scripts with Data Quality Processing functions
      source.dir <- "D:/OneDrive/R Projects/wilko-data-quality/DQ Functions/"
      dq.functions <- list.files(source.dir)
      lapply(paste(source.dir,dq.functions,sep = ""),source)
      
      # Source Directory for the SAP extracts to score
      sap.dir <- ("D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/Input Files")
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
            message(files.to.score[i])
            
            # Read in Data and Subset Columns
            site.data <- read.csv(paste(sap.dir,"/",files.to.score[i],sep = ""),stringsAsFactors = FALSE)
            #site.data <- site.data[,c(1:4,6,10:21)]
            
            # Get correct Size regular expression for PSA 1
            check.size <- get.size(psa1)
            
            # Read in CSV files containing Crticial Attribute Web Types
            get.csf.files(psa1)
            
            # Create DQ columns for format checks
            # If the attribute value is NA then the flag is set to NA
            # Else the attribute format is checked against the Regular Expression
            site.data$size.format <- ifelse(!is.na(site.data$Size),!sapply(site.data$Size,grepl, pattern = check.size),NA)
            site.data$colour.format <- ifelse(!is.na(site.data$Colour),!sapply(site.data$Colour,grepl, pattern = check.colour), NA)
            site.data$pack.qty.format <- ifelse(!is.na(site.data$Pack.Qty),!sapply(site.data$Pack.Qty,grepl, pattern = check.pack.qty), NA)
            site.data$material.format <- ifelse(!is.na(site.data$Material),!sapply(site.data$Material,grepl, pattern = check.material), NA)
            site.data$age.format <- ifelse(!is.na(site.data$Age),!sapply(site.data$Age,grepl, pattern = check.age), NA)
            site.data$assembly.format <- ifelse(!is.na(site.data$Assembly),!sapply(site.data$Assembly,grepl, pattern = check.assembly), NA)
            site.data$capacity.format <- ifelse(!is.na(site.data$Capacity),!sapply(site.data$Capacity,grepl, pattern = check.capacity), NA)
            site.data$coverage.format <- ifelse(!is.na(site.data$Coverage),!sapply(site.data$Coverage,grepl, pattern = check.coverage), NA)
            site.data$washable.format <- ifelse(!is.na(site.data$Washable),!sapply(site.data$Washable,grepl, pattern = check.washable), NA)
            
            # Flag critical attributes that must be populated
            site.data$size.required <- sapply(site.data$Type,function(x) is.element(x,size.required))
            site.data$colour.required <- sapply(site.data$Type,function(x) is.element(x,colour.required))
            site.data$pack.qty.required <- sapply(site.data$Type,function(x) is.element(x,pack.qty.required))
            site.data$material.required <- sapply(site.data$Type,function(x) is.element(x,material.required))
            site.data$age.required <- sapply(site.data$Type,function(x) is.element(x,age.required))
            site.data$assembly.required <- sapply(site.data$Type,function(x) is.element(x,assembly.required))
            site.data$capacity.required <- sapply(site.data$Type,function(x) is.element(x,capacity.required))
            site.data$coverage.required <- sapply(site.data$Type,function(x) is.element(x,coverage.required))
            site.data$washable.required <- sapply(site.data$Type,function(x) is.element(x,washable.required))
            
            site.data$pack.size.required <- ifelse(is.na(site.data$Size) & is.na(site.data$Pack.Qty),TRUE,FALSE)
            
            # Flag critical attributes with missing values
            site.data$size.missing <- ifelse(site.data$size.required == TRUE & is.na(site.data$Size),TRUE,FALSE)
            site.data$colour.missing <- ifelse(site.data$colour.required == TRUE & is.na(site.data$Colour),TRUE,FALSE)
            site.data$pack.qty.missing <- ifelse(site.data$pack.qty.required == TRUE & is.na(site.data$Pack.Qty),TRUE,FALSE)
            site.data$material.missing <- ifelse(site.data$material.required == TRUE & is.na(site.data$Material),TRUE,FALSE)
            site.data$age.missing <- ifelse(site.data$age.required == TRUE & is.na(site.data$Age),TRUE,FALSE)
            site.data$assembly.missing <- ifelse(site.data$assembly.required == TRUE & is.na(site.data$Assembly),TRUE,FALSE)
            site.data$capacity.missing <- ifelse(site.data$capacity.required == TRUE & is.na(site.data$Capacity),TRUE,FALSE)
            site.data$coverage.missing <- ifelse(site.data$coverage.required == TRUE & is.na(site.data$Coverage),TRUE,FALSE)
            site.data$washable.missing <- ifelse(site.data$washable.required == TRUE & is.na(site.data$Washable),TRUE,FALSE)
            site.data$web.description.missing <- sapply(site.data$Web.Description, function(x) is.na(x))
            site.data$brand.missing <- sapply(site.data$Brand, function(x) is.na(x))
            
            # Create DQ columns for consistency checks
            site.data$check.pack.qty <- !(extract.pack.qty(site.data$Web.Description) == site.data$Pack.Qty)
            site.data$check.size <- !(extract.size(site.data$Web.Description) == site.data$Size)
            site.data$check.title.spelling <- ifelse(!is.na(site.data$Web.Description),sapply(site.data$Web.Description,grepl, pattern = web.title.check), NA)
            # Can't work out how to do this one with Apply!
            site.data <- dq.score.title.brand.test(site.data)
            
            
            #Create DQ Scores
            col.missing <- colnames(site.data[grepl("missing",names(site.data))])
            col.format <- colnames(site.data[grepl("format",names(site.data))])
            col.check <- colnames(site.data[grepl("check",names(site.data))])
            
            site.data$missing <- apply(site.data[,col.missing],1,any)
            site.data$format <- apply(site.data[,col.format],1,any)
            site.data$check <- apply(site.data[,col.check],1,any)
            
            site.data$score.missing <- ifelse(site.data$missing == TRUE,dq.score.critical.fail,0)
            site.data$score.format <- ifelse(site.data$format == TRUE,dq.score.partial.fail,0)
            site.data$score.check<- ifelse(site.data$check == TRUE,dq.score.minor.fail,0)
            
            col.score <- colnames(site.data[grepl("score",names(site.data))])
            site.data$score.dq <- 100 + rowSums(site.data[,col.score],na.rm = TRUE)
            site.data$file.name <- paste("File - ", files.to.score[i],sep = "")
            score.file.cols <- c("file.name","Article","PSA_1", "PSA_2", "Type", "Web.Description","score.missing","score.format","score.check","score.dq")
            
            write.csv(site.data[,score.file.cols],paste(report.dir,"Report Data/",files.to.score[i],"_Scored_.csv",sep = ""),row.names = FALSE)
            
      } # End Loop
      
      # Consolidate PSA files into a single file for analysis
      setwd("D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/Report Data")
      options("stringsAsFactors"=FALSE)
      psa.files <- list.files(pattern = '\\.csv')
      psa.tables <- lapply(psa.files, read.csv,header = TRUE)
      site.data <- do.call(rbind , psa.tables)
      write.csv(site.data,paste(report.dir,"Report Data/","SiteData.csv",sep = ""),row.names = FALSE)
      
      
} # End Function

#---------------------------------------------------------------------------------------------------------------
extract.pack.qty <- function(web.description) {
      
      title.pk <- ifelse(grepl("[0-9]+pk$",web.description),
                         sub(".*?([0-9]+).*", "\\1", (sub(".*?([0-9]+pk)$", "\\1", web.description,perl=TRUE)),perl=TRUE), 
                         NA)
      
      return(title.pk)
}


extract.size <- function(web.description) {
      
      title.size <- ifelse(grepl("[0-9]{1,4}(\\.[0-9]{1,2})?(g|ml|kg|L|ft)$",web.description),
                           sub(".*?([0-9]{1,4}(\\.[0-9]{1,2})?(g|ml|kg|L|ft))$", "\\1", web.description,perl=TRUE), 
                           NA)
      
      return(title.size)
}

