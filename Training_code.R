install.packages("camtrapR")
library("camtrapR")

# camtrapR paper: http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12600/full

wd = "E:\\OneDrive - Borneo Nature Foundation\\BNF - Riset Mahasiswa" #Set your working directory where all files are stored
setwd(wd)

exiftoolPath(exiftoolDir = wd) 
#giving location of exiftool for renaming photos and extracting metadata

#--------------------------------------------------------------------------------------
# Renaming photos with metadata
#--------------------------------------------------------------------------------------

rename_table <- imageRename(inDir = "F:\\Hasil Kamera Trap\\TNS",
                            outDir = "F:\\Hasil Kamera Trap\\TNSRenamed",
                            hasCameraFolders = FALSE,
                            copyImages = TRUE)

write.csv(rename_table, file = "rename_table.csv")


#--------------------------------------------------------------------------------------
# Creating a record database after all images have been renamed
#--------------------------------------------------------------------------------------

rec.db2023 <- recordTable(inDir = "F:\\01. CAMERA TRAP\\SBGRenamed",
                      IDfrom = "directory",
                      minDeltaTime = 30,
                      deltaTimeComparedTo = "lastIndependentRecord",
                      timeZone = "Asia/Kuala_Lumpur",
                      writecsv = TRUE,
                      outDir = wd)

species.table <- data.frame(table(rec.db_independent$Species)) #see species and number of records

#--------------------------------------------------------------------------------------
# Producing a Survey Report
#--------------------------------------------------------------------------------------

CTtable = read.csv("CTtable_SBG_2022.csv")
#rec.db = read.csv("rec_db.csv", row.names = 1)

report <- surveyReport(recordTable = rec.db_all,
                       CTtable = tn_loc,
                       speciesCol = "Indonesia",
                       stationCol = "Station",
                       setupCol = "Setup",
                       retrievalCol = "Collection",
                       CTDateFormat = "%d/%m/%Y", 
                       recordDateTimeCol = "DateTimeOriginal",
                       recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                       Xcol = "X",
                       Ycol = "Y",
                       sinkpath = wd,
                       makezip = TRUE)


#--------------------------------------------------------------------------------------
# Adding Information
#--------------------------------------------------------------------------------------

rec.db = read.csv("record_database.csv", row.names = 1)
ref = read.csv("reference_table.csv", na.strings = "-")

scientific.name = rep(NA, nrow(rec.db))
rec.db = cbind(rec.db[,1:2], scientific.name, rec.db[,3:11])

for (i in 1:nrow(ref)){
  rec.db$scientific.name[as.character(rec.db$Species) == as.character(ref$Species)[i]] = as.character(ref$Scientific.name[i])
}

#--------------------------------------------------------------------------------------
# Summary Table Function w/ Naive Occupancy and Relative Abundance Index
#--------------------------------------------------------------------------------------

sumtable = function(rec.db, effort){
  
  stations <- c(length(unique(rec.db$Station)))
  sp <- c(as.character(unique(rec.db$Species)))
  table <- data.frame(Species = sp, 
                      No.detections = rep(NA, length(sp)), 
                      Stations.detected = rep(NA, length(sp)),
                      Naive.occu = rep(NA, length(sp)),
                      RAI = rep(NA, length(sp))
  )
  
  for (i in 1:length(sp)) {
    
    table$No.detections[i] <- nrow(subset(rec.db, rec.db$Species == sp[i]))
    table$Stations.detected[i] <- length(unique(subset(rec.db, rec.db$Species == sp[i])$Station))
    table$Naive.occu[i] <- table$Stations.detected[i]/stations
    table$RAI[i] <- (table$No.detections[i]/effort)*100
    
  }
  
  return(table)
  
}

summary.table = sumtable(rec.db, 807)

write.csv(summary.table, file = "summary_table.csv")


#--------------------------------------------------------------------------------------
# Changing Entry/Record names
#--------------------------------------------------------------------------------------

rec.db$Species[rec.db$Species == "Birtds"] <- "Bird"
rec.db$Species[rec.db$Species == "Birts"] <- "Bird"
rec.db$Species[rec.db$Species == "Bornean Yellow Muntjac"] <- "Bornean Yellow Muntjac Deer"
rec.db$Species[rec.db$Species == "Oter"] <- "Otter"
rec.db$Species[rec.db$Species == "Pig-Tailed Macaque"] <- "Pig-tailed Macaque"
rec.db$Species[rec.db$Species == "Pig Tailed Macaque"] <- "Pig-tailed Macaque"
rec.db$Species[rec.db$Species == "Unknow"] <- "Unknouwn"
rec.db$Species[rec.db$Species == "Unknown"] <- "Unknouwn"
rec.db$Species[rec.db$Species == "Yellow Muntjac"] <- "Bornean Yellow Muntjac Deer"

#--------------------------------------------------------------------------------------
# Removing records
#--------------------------------------------------------------------------------------

rec.db <- rec.db[!(rec.db$Species == "Forest"),]
rec.db <- rec.db[!(rec.db$Species == "Human"),]
rec.db <- rec.db[!(rec.db$Species == "Unknown"),]
rec.db <- rec.db[!(rec.db$X       == "239"),]


#--------------------------------------------------------------------------------------
# Subsetting Information
#--------------------------------------------------------------------------------------

dat.sunbear = subset(rec.db, rec.db$Species == "Sun bear")

dat.psn119 = subset(rec.db, rec.db$Station == "PSN119")

