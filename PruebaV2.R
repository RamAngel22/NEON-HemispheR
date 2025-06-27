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

# Selecciona el sitio y la fecha
set.siteID <- "SCBI"
set.year <- 2021
set.month <- 8

lessData <- subset(x = data, subset = data$siteID == set.siteID & data$year == set.year & data$month == set.month)

# Checa eventID and plotID disponibles en ese sitio y fecha
unique(lessData$eventID)
unique(lessData$plotID)

# Seleccionar uno de cada uno para descargar
set.eventID <- "dhp.2021.11.SCBI.TOWER"
set.plotID <- "SCBI_063"
eventPlotData <- subset(x = data, subset = data$siteID == set.siteID & data$eventID == set.eventID & data$plotID == set.plotID)

#Remover la data ya no necesaria para esta parte
rm(data)
rm(lessData)

#Crear y escribir direccion de folder de las fotos
photo_dir <- file.path(script_dir, "NEON_hemispheric-photos-veg", "stackedFiles", "tmp_photos_test")
if (!dir.exists(photo_dir)) {
  dir.create(photo_dir)
}
dir.create("C:/DHP/results", recursive = TRUE, showWarnings = FALSE)
setwd("C:/DHP/results")

#Los resultados antes de los datos
results <- list()


# Descarga fotos para evento/plot, convierte a jpg y corre todas las funciones de hemispheR
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
                      message=TRUE)
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
    message = TRUE)
  rm(img.bw)
  print("Step 6/6")
  canopy<-canopy_fisheye(gap.frac)
  canopy$id <- eventPlotData$subsampleID[i]
  canopy$eventID <- set.eventID
  canopy$plotID <- set.plotID
  results[[i]] <- canopy
  rm(gap.frac, canopy)  # Remove large objects
  gc()  # Run garbage collection to free memory
}

#Todos los resultados en un csv
final_results <- do.call(rbind, results)
result_file <- file.path(script_dir, paste0(set.eventID, "_", set.plotID, "_results.csv"))
write.csv(final_results, result_file, row.names = FALSE)