
count.changes <- function(psa1) {

library(dplyr)

psa1.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1,"/Uploads/",sep="")      
      
      
update.age.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Age.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.assembly.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Assembly.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.brand.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Brand.csv",sep = ""),header = TRUE)
update.capacity.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Capacity.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.coverage.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Coverage.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.model.no.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Model.Number.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.size.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Size.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.pack.qty.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Pack.Qty.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.colour.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Colour.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.material.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Material.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.web.description.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Web.Description.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
update.washable.data <<- read.csv(paste(psa1.dir,psa1,"_Updates_Washable.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)


update.brand.data <- update.brand.data[,c("PSA_1","PSA_2","Article","Web.Description","Web.Description","Brand","Title.Brand")]
colnames(update.brand.data) <- c("PSA_1","PSA_2","Article","Web.Description","Old.Value","New.Value","Change")
update.brand.data$Change <- "Update"


a <<- rbind(update.age.data,update.assembly.data,update.capacity.data,update.coverage.data,update.model.no.data,
           update.size.data,update.pack.qty.data,update.colour.data,update.material.data,update.web.description.data,
           update.washable.data,update.brand.data)


b <<- a[a$Change != "Keep",]

change.file <<- data.frame(count(b,b$Change))

write.csv(change.file,paste(psa1.dir,"Total_Changes.csv",sep = ""))

}