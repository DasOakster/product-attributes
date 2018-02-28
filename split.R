#rm(list= ls()[!(ls() %in% c("toi.products","toi.original","size.change","blanks","keeps","toi.compare"))]) 



split.files <- function(colatr) {
      
      psa2 <- unique(toi.compare$PSA_2.x)

      for(q in 1:NROW(psa2)) {
            
            x <- psa2[q]
            output.file <- subset(toi.compare, PSA_2.x == x)
            output.file <- subset(output.file,Change ==  "Infill" | Change == "Update" | Change == "Blank")
            assign(paste(colatr,".change.",x,sep = ""),output.file) 
      
      }

}


