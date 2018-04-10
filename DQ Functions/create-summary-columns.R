# These functions check each of the Score columns and flag as FAIL if any score less than Zero
#----------------------------------------------------------------------------------------------------------------------
score.size <- function (product.data) {

product.data$Size.DQ <- NA

df.columns <- colnames(product.data)

size.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Size",df.columns)==TRUE)])

for(i in 1:NROW(product.data)){

      if(rowSums(product.data[i,size.cols]) < dq.score.minor.fail) {
            
            product.data$Size.DQ[i] = "FAIL"
      }   
      
      else {
            product.data$Size.DQ[i] = "PASS"
      } 
           
}

      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Size.DQ")
      s <- subset(product.data,select = report.cols)
      s$Attribute <- "Size"
      colnames(s)[colnames(s)=="Size.DQ"] <- "Status"

      s <<- s
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------
score.pack.qty <- function (product.data) {
      
      product.data$Pack.Qty.DQ <- NA
      
      df.columns <- colnames(product.data)
      
      pack.cols <- c(df.columns[(grepl("Score",df.columns)==TRUE & grepl("Pack",df.columns)==TRUE)])
      
      for(i in 1:NROW(product.data)){
            
            if(rowSums(product.data[i,pack.cols]) < dq.score.minor.fail) {
                  
                  product.data$Pack.Qty.DQ[i] = "FAIL"
            }   
            
            else {
                  product.data$Pack.Qty.DQ[i] = "PASS"
            } 
            
      }
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Pack.Qty.DQ")
      p <- subset(product.data,select = report.cols)
      p$Attribute <- "Pack.Qty"
      colnames(p)[colnames(p)=="Pack.Qty.DQ"] <- "Status"
      
      p <<- p
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Colour.DQ")
      cr <- subset(product.data,select = report.cols)
      cr$Attribute <- "Colour"
      colnames(cr)[colnames(cr)=="Colour.DQ"] <- "Status"
      
      cr <<- cr
      
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
      
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Material.DQ")
      m <- subset(product.data,select = report.cols)
      m$Attribute <- "Material"
      colnames(m)[colnames(m)=="Material.DQ"] <- "Status"
      
      m <<- m
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Power.DQ")
      po <- subset(product.data,select = report.cols)
      po$Attribute <- "Power"
      colnames(po)[colnames(po)=="Power.DQ"] <- "Status"
      
      po <<- po
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Capacity.DQ")
      ca <- subset(product.data,select = report.cols)
      ca$Attribute <- "Capacity"
      colnames(ca)[colnames(ca)=="Capacity.DQ"] <- "Status"
      
      ca <<- ca
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Coverage.DQ")
      co <- subset(product.data,select = report.cols)
      co$Attribute <- "Coverage"
      colnames(co)[colnames(co)=="Coverage.DQ"] <- "Status"
      
      co <<- co
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Age.DQ")
      ag <- subset(product.data,select = report.cols)
      ag$Attribute <- "Age"
      colnames(ag)[colnames(ag)=="Age.DQ"] <- "Status"
      
      ag <<- ag
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Washable.DQ")
      w <- subset(product.data,select = report.cols)
      w$Attribute <- "Washable"
      colnames(w)[colnames(w)=="Washable.DQ"] <- "Status"
      
      w <<- w
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Model.DQ")
      mo <- subset(product.data,select = report.cols)
      mo$Attribute <- "Model.No"
      colnames(mo)[colnames(mo)=="Model.DQ"] <- "Status"
      
      mo <<- mo
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Brand.DQ")
      b <- subset(product.data,select = report.cols)
      b$Attribute <- "Brand"
      colnames(b)[colnames(b)=="Brand.DQ"] <- "Status"
      
      b <<- b
      
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
      
      report.cols <- c("PSA_1","PSA_2","Article","Web.Description","Type","Assembly.DQ")
      ay <- subset(product.data,select = report.cols)
      ay$Attribute <- "Assembly"
      colnames(ay)[colnames(ay)=="Assembly.DQ"] <- "Status"
      
      ay <<- ay
      
      return(product.data)
      
}
#----------------------------------------------------------------------------------------------------------------------


