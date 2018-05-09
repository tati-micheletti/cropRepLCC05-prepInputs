
defineModule(sim, list(
  name = "prepInputsProblems",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", prepInputsProblems = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "prepInputsProblems.Rmd"),
  reqdPkgs = list("sp", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "polyMatrix", objectClass = "matrix", desc = "For 'cropTemplate = rP', this is the latlong center point for the random polygon", sourceURL = NA),
    expectsInput(objectName = "areaSize", objectClass = "numeric", desc = "For 'cropTemplate = rP', this is the size in hactares of the random polygon", sourceURL = NA)
),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
#    createsOutput(objectName = "template", objectClass = "RasterLayer", desc = "Map working"),
    createsOutput(objectName = "studyArea1", objectClass = "shapefile", desc = "sA working"),
    createsOutput(objectName = "studyArea2", objectClass = "shapefile", desc = "sA not working"),
    createsOutput(objectName = "map1", objectClass = "RasterLayer", desc = "Map working"),
    createsOutput(objectName = "map2", objectClass = "RasterLayer", desc = "Map not working: rasterToMatch, different from template"),
    createsOutput(objectName = "map3", objectClass = "RasterLayer", desc = "Map not working: rasterToMatch, same as template"),
    createsOutput(objectName = "map4", objectClass = "RasterLayer", desc = "Map not working: Cache"),
    createsOutput(objectName = "map5", objectClass = "RasterLayer", desc = "Map not working: Cache"),
    createsOutput(objectName = "map6", objectClass = "RasterLayer", desc = "Map not working: Cache"),
    createsOutput(objectName = "map7", objectClass = "RasterLayer", desc = "Map not working: Cache"))
))

## event types
#   - type `init` is required for initialiazation

doEvent.prepInputsProblems = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
          template <- prepInputs(url = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip", destinationPath = dataPath(sim)) # OUTSIDE sim (temporary) --> Caching in cache folder and keep a copy in dataPath()
      
      sim$studyArea1 <- randomPolygon(x = sim$polyMatrix, hectares = sim$areaSize) %>%
        postProcess(targetFilePath = dataPath(sim), destinationPath = dataPath(sim)) # Not using rasterToMatch: works
      
      # sim$studyArea2 <- randomPolygon(x = sim$polyMatrix, hectares = sim$areaSize) %>%
      #   postProcess(targetFilePath = dataPath(sim), destinationPath = dataPath(sim), rasterToMatch = template) # rasterToMatch: doesn't work; doesn't finish nor returns error.
      
      sim$studyArea3 <- prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                  destinationPath = dataPath(sim)) #, rasterToMatch also fails here.
      sim$studyArea3 <- sim$studyArea3[sim$studyArea3$ECODISTRIC == 339,] %>%
          sp::spTransform(CRSobj = proj4string(template))
      
      sim$map1 <- prepInputs(url = paste0("http://www.cec.org/sites/default/files/Atlas/Files/",
                                          "Land_Cover_2010/Land_Cover_2010_TIFF.zip"),
                             targetFile = "NA_LandCover_2010_25haMMU.tif",
                             destinationPath = dataPath(sim),
                             studyArea = sim$studyArea1) # works but not the same projection as template (obviously). Different map.
      
      #============= STUDY AREA 1 (random polygon)

      # sim$map2 <- prepInputs(url = paste0("http://www.cec.org/sites/default/files/Atlas/Files/",
      #                                     "Land_Cover_2010/Land_Cover_2010_TIFF.zip"),
      #                        targetFile = "NA_LandCover_2010_25haMMU.tif",
      #                        destinationPath = dataPath(sim),
      #                        studyArea = sim$studyArea1,
      #                        rasterToMatch = template) # did not work, returns NULL, fails silently. Different map.
      
      # sim$map3 <- prepInputs(url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
      #                                     "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
      #                        destinationPath = dataPath(sim),
      #                        studyArea = sim$studyArea1,
      #                        rasterToMatch = template) # did not work, returns NULL, fails silently. Same map. 
      
      # sim$map4 <- prepInputs(url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
      #                                     "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
      #                        destinationPath = dataPath(sim),
      #                        studyArea = sim$studyArea1) # works. Same map
      
      #============= STUDY AREA 3 (downloaded shapefile)
      
      # sim$map5 <- prepInputs(url = paste0("http://www.cec.org/sites/default/files/Atlas/Files/",
      #                                     "Land_Cover_2010/Land_Cover_2010_TIFF.zip"),
      #                        targetFile = "NA_LandCover_2010_25haMMU.tif",
      #                        destinationPath = dataPath(sim),
      #                        studyArea = sim$studyArea3,
      #                        rasterToMatch = template) # did not work, returns NULL, fails silently. Different map.
      
      # sim$map6 <- prepInputs(url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
      #                                     "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
      #                        destinationPath = dataPath(sim),
      #                        studyArea = sim$studyArea1,
      #                        rasterToMatch = template) # did not work, returns NULL, fails silently. Same map.
      
      # sim$map7 <- prepInputs(url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
      #                                     "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
      #                        destinationPath = dataPath(sim),
      #                        studyArea = sim$studyArea1) # works. Same map
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  sim$polyMatrix <- matrix(c(-122.85, 52.04), ncol = 2)
  sim$areaSize <- 500000
  
  return(invisible(sim))
}
