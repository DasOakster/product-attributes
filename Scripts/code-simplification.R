data.quality.reports <- function() {
      
      # Read in Scripts with Data Quality Processing functions
      source.dir <- "D:/OneDrive/R Projects/product-attributes/DQ Functions/"
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
            csf.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/PSA Folders/",psa1,"/CSF Files/",sep = "")
            psa1.site.data <- read.csv(paste(sap.dir,"/",files.to.score[i],sep = ""))
            psa1.site.data <- psa1.site.data[,c(1:4,6,10:21)]
            
            # Get correct Size regular expression for PSA 1
            get.size(psa1)

            # Read in Web Product Types that require 100% completion
            colour.required <<- read.csv(paste(csf.dir,"CSF_Colour.csv",sep = ""))[,1]
            size.required <<- read.csv(paste(csf.dir,"CSF_Size.csv",sep = ""))[,1]
            pack.qty.required <<- read.csv(paste(csf.dir,"CSF_PackQty.csv",sep = ""))[,1]
            age.required <<- read.csv(paste(csf.dir,"CSF_Age.csv",sep = ""))[,1]
            assembly.required <<- read.csv(paste(csf.dir,"CSF_Assembly.csv",sep = ""))[,1]
            material.required <<- read.csv(paste(csf.dir,"CSF_Material.csv",sep = ""))[,1]
            washable.required <<- read.csv(paste(csf.dir,"CSF_Washable.csv",sep = ""))[,1]
            coverage.required <<- read.csv(paste(csf.dir,"CSF_Coverage.csv",sep = ""))[,1]
            capacity.required <<- read.csv(paste(csf.dir,"CSF_Capacity.csv",sep = ""))[,1]
            power.required <<- read.csv(paste(csf.dir,"CSF_Power.csv",sep = ""))[,1]
            model.number.required <<- read.csv(paste(csf.dir,"CSF_ModelNumber.csv",sep = ""))[,1]
            
            return(psa1.site.data)
            
            } # End Loop

} # End Function