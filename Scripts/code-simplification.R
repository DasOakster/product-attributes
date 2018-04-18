data.quality.reports <- function() {

# This function is designed to take a data extract from either SAP or Wilko.com and run the Data Quality checks on the Attribute fields
# These checks include: Format, Completeness and Consistency
# Scores out of 100 are allocated to each product
# Scored files are then exported for use in Excel Data Quality reports
      
            
      # Read in Scripts with Data Quality Processing functions
      source.dir <- "D:/OneDrive/R Projects/wilko-data-quality/DQ Functions/"
      dq.functions <- list.files(source.dir)
      lapply(paste(source.dir,dq.functions,sep = ""),source)
      
      # Source Directory for the SAP extracts to score
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
            site.data <- read.csv(paste(sap.dir,"/",files.to.score[i],sep = ""))
            site.data <- site.data[,c(1:4,6,10:21)]
            
            # Get correct Size regular expression for PSA 1
            check.size <- get.size(psa1)

            # Read in CSV files containing Crticial Attribute Web Types
            get.csf.files(psa1)
            
            # Create DQ columns for format checks
            # If the attribute value is NA then the flag is set to NA
            # Else the attribute format is checked against the Regular Expression
            site.data$size.format <- ifelse(!is.na(site.data$Size),sapply(site.data$Size,grepl, pattern = check.size),NA)
            site.data$colour.format <- ifelse(!is.na(site.data$Colour),sapply(site.data$Colour,grepl, pattern = check.colour), NA)
            site.data$pack.qty.format <- ifelse(!is.na(site.data$Pack.Qty),sapply(site.data$Pack.Qty,grepl, pattern = check.pack.qty), NA)
            site.data$material.format <- ifelse(!is.na(site.data$Material),sapply(site.data$Material,grepl, pattern = check.material), NA)
            site.data$age.format <- ifelse(!is.na(site.data$Age),sapply(site.data$Age,grepl, pattern = check.age), NA)
            site.data$assembly.format <- ifelse(!is.na(site.data$Assembly),sapply(site.data$Assembly,grepl, pattern = check.assembly), NA)
            site.data$capacity.format <- ifelse(!is.na(site.data$Capacity),sapply(site.data$Capacity,grepl, pattern = check.capacity), NA)
            site.data$coverage.format <- ifelse(!is.na(site.data$Coverage),sapply(site.data$Coverage,grepl, pattern = check.coverage), NA)
            site.data$washable.format <- ifelse(!is.na(site.data$Washable),sapply(site.data$Washable,grepl, pattern = check.washable), NA)
            
            # Create DQ columns for completeness checks
            site.data$size.required <- sapply(site.data$Type,function(x) is.element(x,size.required))
            site.data$colour.required <- sapply(site.data$Type,function(x) is.element(x,colour.required))
            site.data$pack.qty.required <- sapply(site.data$Type,function(x) is.element(x,pack.qty.required))
            site.data$material.required <- sapply(site.data$Type,function(x) is.element(x,material.required))
            site.data$age.required <- sapply(site.data$Type,function(x) is.element(x,age.required))
            site.data$assembly.required <- sapply(site.data$Type,function(x) is.element(x,assembly.required))
            site.data$capacity.required <- sapply(site.data$Type,function(x) is.element(x,capacity.required))
            site.data$coverage.required <- sapply(site.data$Type,function(x) is.element(x,coverage.required))
            site.data$washable.required <- sapply(site.data$Type,function(x) is.element(x,washable.required))
            site.data$web.description.required <- sapply(site.data$Web.Description, function(x) !is.na(x))
            
            # Create DQ columns for consistency checks
            site.data$check.web.description <- sapply(site.data$Web.Description, function(x) is.na(x))
            
            titlepk <- get.pack.qty(site.data$Web.Description)
            
           # site.data$inconsistent.pack.qty <- ifelse(grepl("[0-9]+pk$",site.data$Web.Description), 
            #                                          sapply(site.data$Pack.Qty, 
             #                                         function(x) !identical(x,titlepk)), NA)
            
            
            site.data$check.pack.qty <- ifelse(grepl("[0-9]+pk$",site.data$Web.Description),
                                               sapply(site.data$Pack.Qty, 
                                                function(x) title.pk),NA)
            
            
            #format.cols <- colnames(a[,grepl("format",names(a))])
            #required.cols <- colnames(a[,grepl("required",names(a))])
            
            return(site.data)
            
            } # End Loop

} # End Function

get.pack.qty <- function(web.description) {

      title.pk <- ifelse(grepl("[0-9]+pk$",web.description),
                         sub(".*?([0-9]+).*", "\\1", (sub(".*?([0-9]+pk)$", "\\1", web.description,perl=TRUE)),perl=TRUE), 
                         NA)
      
      return(as.numeric(title.pk))
}
