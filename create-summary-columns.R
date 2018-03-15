# These functions check each of the Score columns and flag as FAIL if any score less than Zero
#----------------------------------------------------------------------------------------------------------------------
score.size <- function (product.data) {

product.data$Size.DQ <- NA

df.columns <- colnames(product.data)

size.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Size",df.columns)==TRUE)])

for(i in 1:NROW(product.data)){

      if(rowSums(product.data[i,size.cols]) < 0) {
            
            product.data$Size.DQ[i] = "FAIL"
      }   
      
      else {
            product.data$Size.DQ[i] = "PASS"
      } 
           
}

      return(product.data)

}
#----------------------------------------------------------------------------------------------------------------------
score.pack.qty <- function (product.data) {
      
      product.data$Pack.Qty.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      pack.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Pack",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,pack.cols]) < 0) {
                  
                  product.data$Pack.Qty.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Pack.Qty.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.colour <- function (product.data) {
      
      product.data$Colour.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      colour.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Colour",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,colour.cols]) < 0) {
                  
                  product.data$Colour.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Colour.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.material <- function (product.data) {
      
      product.data$Material.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      material.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Material",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,material.cols]) < 0) {
                  
                  product.data$Material.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Material.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.power <- function (product.data) {
      
      product.data$Power.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      power.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Power",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,power.cols]) < 0) {
                  
                  product.data$Power.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Power.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.capacity <- function (product.data) {
      
      product.data$Capacity.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      capacity.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Capacity",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,capacity.cols]) < 0) {
                  
                  product.data$Capacity.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Capacity.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.coverage <- function (product.data) {
      
      product.data$Coverage.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      coverage.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Coverage",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,coverage.cols]) < 0) {
                  
                  product.data$Coverage.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Coverage.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.age <- function (product.data) {
      
      product.data$Age.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      age.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Age",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,age.cols]) < 0) {
                  
                  product.data$Age.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Age.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.washable <- function (product.data) {
      
      product.data$Washable.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      washable.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Washable",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,washable.cols]) < 0) {
                  
                  product.data$Washable.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Washable.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.model <- function (product.data) {
      
      product.data$Model.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      model.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Model",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,model.cols]) < 0) {
                  
                  product.data$Model.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Model.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.brand <- function (product.data) {
      
      product.data$Brand.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      brand.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Brand",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,brand.cols, drop = FALSE]) < 0) {
                  
                  product.data$Brand.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Brand.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.assembly <- function (product.data) {
      
      product.data$Assembly.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      assembly.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Assembly",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,assembly.cols]) < 0) {
                  
                  product.data$Assembly.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Assembly.DQ[i] = "PASS"
            } 
            
      }
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
