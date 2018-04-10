# Function to create output files of changes by attribute
compare.attributes <- function(original.df,updated.df,psa1) {
      
      attribute.file <- merge(original.df,updated.df,by.x = "Article",by.y = "Article")
      attributes <- c("Size",
                      "Colour",
                      "Pack.Qty",
                      "Material",
                      "Assembly",
                      "Washable",
                      "Power",
                      "Capacity",
                      "Coverage",
                      "Age",
                      "Model.Number",
                      "Web.Description")
      
      attribute.file$Change <- NA

      for(a in 1:NROW(attributes)) {
      
            # Create a df with the two attribute columns to be compared
            attribute <- attributes[a]
            attribute.x <- paste(attributes[a],".x",sep = "")
            attribute.y <- paste(attributes[a],".y",sep = "")
            attribute.compare <- attribute.file[,c("PSA_1.x","PSA_2.x","Article","Web.Description.x",attribute.x,attribute.y,"Change")]

            for(i in 1:NROW(attribute.compare)) {
            
                  original.value <- as.character(attribute.compare[i,attribute.x])
                  update.value <- as.character(attribute.compare[i,attribute.y])
                  
                  # Orignal NA replaced by a value
                  if(is.na(original.value) && !is.na(update.value)) { 
                        
                        attribute.compare$Change[i] <- "Infill"
                  }
                  
                  # Original and Updated are both NA
                  else if (is.na(original.value) && is.na(update.value)) { 
                        
                        attribute.compare$Change[i] <- "Keep"
                        
                  }
                  
                  # Original has a value but update removes it
                  else if (!is.na(original.value) && is.na(update.value)) {
                        
                        attribute.compare$Change[i] <- "Blank"     
                  }
                  
                  # Original and update are the same value
                  else if (as.character(original.value) == as.character(update.value)) {
                        
                        attribute.compare$Change[i] <- "Keep"     
                  }
                  
                  # Otherwise update the value
                  else  {
                        
                        attribute.compare$Change[i] <- "Update"     
                  }     
                  
                  
            } # End sub-loop
      
      
         
      #Split files by PSA2 and save them as CSV files
                  
      split.files(attribute.compare,updated.df,attribute,psa1)
                  
      # Split file by PSA2 and save the ones requiring a Brand check as CSV files
                  
      brand.files(psa1)
            
      } # End main loop

} # End function



split.files <- function(attribute.compare, psa1.update.data, attribute,psa1) {
      
# Export file of attribute comparison data
      
      export.file.name <- paste(psa1,"_Updates_",attribute,".csv",sep = "")
      colnames(attribute.compare) <- c("PSA_1","PSA_2","Article","Web.Description","Old.Value","New.Value","Change")
      write.csv(attribute.compare,paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1,"/Uploads/",export.file.name,sep = ""),row.names = FALSE)   
      
#Split files by PSA2 and Attribute
      
      all.psa2 <- unique(attribute.compare$PSA_2)
      
      for(i in 1:NROW(all.psa2)) {
            
            psa2 <- all.psa2[i]
            psa2.file <- subset(attribute.compare, PSA_2 == psa2)
            
            update.file <- subset(psa2.file,Change ==  "Infill" | Change == "Update" | Change == "Blank")
            
            check.file <- subset(psa1.update.data,Title.Spelling == "CHECK" | Title.Brand == "CHECK" | Title.Size == "CHECK" | Title.Pack.Qty == "CHECK")
            check.file <- subset(check.file[,c("PSA_1","PSA_2","Article","Web.Description","Brand","Size","Pack.Qty","Title.Spelling","Title.Brand","Title.Size","Title.Pack.Qty")])
            
            psa1.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1, "/",sep = "")
            psa2.dir <- paste(psa1.dir,"PSA2/",psa2, sep = "")
            
            if(!dir.exists(psa2.dir)) {
            
                        dir.create(psa2.dir,showWarnings = FALSE)
            }                  
                  
            setwd(psa2.dir)
            update.cases <- NROW(update.file)
            check.cases <- NROW(check.file)
            
            if(update.cases > 0) write.csv(update.file,paste(psa2,"_Update_",attribute," (",update.cases, ")",".csv",sep = ""),row.names = FALSE)
            if(check.cases > 0) write.csv(check.file,paste(psa2,"_Check_Web_Description (",check.cases, ")",".csv",sep = ""),row.names = FALSE)
    
            setwd("..")
      }
      
}

brand.files <- function(psa1) {
      
      psa2 <- unique(psa1.update.data$PSA_2)
      psa1.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1,"/",sep = "")
      
      all.brand.issues <- subset(psa1.update.data,(Title.Brand == "FAIL"))
      write.csv(all.brand.issues,paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1,"/Uploads/",psa1,"_Updates_Brand.csv",sep=""),row.names = FALSE) 

      for(i in 1:NROW(psa2)) {
            
            brand.issue <- subset(psa1.update.data,(PSA_2 == psa2[i] & Title.Brand == "FAIL"))
            setwd(paste(psa1.dir,"PSA2/",psa2[i],sep = ""))
            num.cases <- NROW(brand.issue)
            brand.issue <- brand.issue[,c("PSA_1","PSA_2","Article","Web.Description","Brand")]
            write.csv(brand.issue,paste(psa2[i],"_Check_Brand_","(",num.cases,").csv",sep = ""),row.names = FALSE)
            
      }
}

