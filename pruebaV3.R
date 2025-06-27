#Instala Librerias
install.packages("neonUtilities")
install.packages("plyr")
install.packages("rstudioapi")
install.packages('hemispheR')

#Carga Librerias
library(neonUtilities)
library(plyr)
library(rstudioapi)
library(hemispheR)


#Consigure direccion de la carpeta donde esta el codigo R
file_path <- normalizePath(rstudioapi::getActiveDocumentContext()$path)
script_dir <- dirname(file_path)
print(script_dir)

#El zip de NEON o el folder extraiedo en la misma cartpeta
zip_file <- file.path(script_dir, "NEON_hemispheric-photos-veg.zip")
stackByTable(filepath = zip_file, saveUnzippedFiles = FALSE)

folder_file <- file.path(script_dir, "NEON_hemispheric-photos-veg")
stackByTable(filepath = folder_file, folder = TRUE, saveUnzippedFiles = FALSE)

# Carga tablas y poner fecha en formato
data_file <- file.path(script_dir, "NEON_hemispheric-photos-veg", "stackedFiles", "dhp_perimagefile.csv")
var_file <- file.path(script_dir, "NEON_hemispheric-photos-veg", "stackedFiles", "variables_10017.csv")
data <- readTableNEON(dataFile = data_file, varFile = var_file)
dates <- as.POSIXlt(data$startDate)
data$year <- dates$year + 1900
data$month <- dates$mon + 1
data$monthday <- dates$mday
data$jday <- dates$yday + 1

#Seccionar el dato
plot_file <-  file.path(script_dir, "NEON_hemispheric-photos-veg", "stackedFiles", "dhp_perplot.csv")
data.plot <- readTableNEON(dataFile = plot_file, varFile = var_file)
data.plot <- subset(x = data.plot, select = c("eventID", "sampleID"))
data <- join(x = data, y = data.plot, by = "sampleID")

subdata_set <- file.path(script_dir, "perimagefilefinal.csv")
n_sub <- nrow(subdata_set)
second_half_sub <- subdata_set[(floor(n_sub / 2) + 1):n_sub, ]

lessData <- subset(x = data, subset = data$uid %in% second_half_small$uid)

rm(data)

plot_list <- as.list(unique(lessData$plotID))

setwd("C:/DHP/results")

for(j in seq_along(plot_list)){
  total_plots <- length(plot_list)
  print(paste("Processing plot", j, "of", total_plots))

  set.plotID <- plot_list[[j]]
  eventPlotData <- subset(x = lessData, subset = lessData$plotID == set.plotID)

  photo_dir <- file.path(script_dir, paste0(set.plotID))
  if (!dir.exists(photo_dir)) {
    dir.create(photo_dir)
  }

  results <- list()

  for (i in seq_len(nrow(eventPlotData))){
    print(paste("Processing image", i, "of", nrow(eventPlotData)))
    print("Step 1/6")
    dest_file <- file.path(photo_dir, eventPlotData$imageFileName[i])
    download.file(url = eventPlotData$imageFileUrl[i], destfile = dest_file, mode = "wb")
    print("Step 2/6")
    cmd <- sprintf('magick mogrify -contrast-stretch 1x99%% -format jpg "%s"', dest_file)
    system(cmd)
    dest_jpg <- sub("\\.NEF$", ".jpg", dest_file)
    file.remove(dest_file)
    new_name <- file.path(photo_dir, paste0(eventPlotData$subsampleID[i], ".jpg"))
    file.rename(dest_jpg, new_name)
    print("Step 3/6")
    img<-import_fisheye(new_name,
                        circular=FALSE,
                        gamma=1,
                        display=FALSE,
                        message=FALSE)
    print("Step 4/6")
    img.bw<-binarize_fisheye(img,
                             method='Otsu',
                             zonal=FALSE,
                             manual=NULL,
                             display=FALSE,
                             export=TRUE)
    rm(img)
    res_dir <- file.path("C:/DHP/results/results", paste0("class_", eventPlotData$subsampleID[i], ".jpg"))
    bw_name <- file.path(photo_dir, paste0(eventPlotData$subsampleID[i], "_BW.jpg"))
    file.rename(from = res_dir, to = bw_name)
    print("Step 5/6")
    gap.frac<-gapfrac_fisheye(
      img.bw,
      maxVZA = 90,
      lens = "FC-E8",
      startVZA = 0,
      endVZA = 70,
      nrings = 7,
      nseg = 8,
      display = FALSE,
      message = FALSE)
    rm(img.bw)
    print("Step 6/6")
    canopy<-canopy_fisheye(gap.frac)
    canopy$id <- eventPlotData$subsampleID[i]
    canopy$eventID <- eventPlotData$eventID[i]
    canopy$plotID <- set.plotID
    canopy$startDate <- eventPlotData$startDate[i]
    canopy$endDate <- eventPlotData$endDate[i]
    results[[i]] <- canopy
    rm(gap.frac, canopy)  # Remove large objects
    gc()  # Run garbage collection to free memory
  }
  final_results <- do.call(rbind, results)
  result_file <- file.path(script_dir, paste0(set.plotID, "_results.csv"))
  write.csv(final_results, result_file, row.names = FALSE)
}


