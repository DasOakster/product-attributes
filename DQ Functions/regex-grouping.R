get.size <- function(psa1) {
      
      # Allocate correct Regular Expressions for PSA1
      if(psa1 == "TOI") {
            
            check.size <- paste(check.weight,
                                check.hygiene,
                                check.nappy,
                                check.bandage,
                                check.tissue,
                                check.measure,
                                check.fit,
                                sep="|")
      }
      
      else if(psa1 == "CLE") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                sep="|")
      }
      
      
      else if(psa1 == "DIS") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                sep="|")
      }
      
      else if(psa1 == "GAR") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                check.fit,
                                check.engine.size,
                                check.electrical,
                                check.shoe.size,
                                sep="|")
      }
      
      else if(psa1 == "KIT") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                check.fit,
                                check.drawer,
                                check.electrical,
                                sep="|")
      }
      
      else if(psa1 == "HOM") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                check.fit,
                                check.drawer,
                                check.bedding,
                                check.electrical,
                                sep="|")
      }
      
      else {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                sep="|")
      }
      
      return(check.size)
      
}


get.csf.files <- function(psa1){
      
      csf.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/PSA Folders/",psa1,"/CSF Files/",sep = "")
      
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
      
}
