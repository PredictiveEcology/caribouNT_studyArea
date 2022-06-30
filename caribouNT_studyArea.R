defineModule(sim, list(
  name = "caribouNT_studyArea",
  description = paste("Prepares 2 sets of objects needed for LandR-fireSense simulations in the Northwest Territories, Canada:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions);",
                      "2. species equivalencies tables and the sppEquiv column;",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = c(
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut"),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "aut"),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "ctb"),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = "ctb")
  ),
  childModules = character(0),
  version = list(caribouNT_studyArea = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "caribouNT_studyArea.Rmd")),
  reqdPkgs = list("magrittr", "raster", "sf", "sp",
                  "PredictiveEcology/reproducible@development (>= 1.2.8.9033)",
                  "PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9014)",
                  "PredictiveEcology/LandR@development"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("bufferDist", "numeric", 20000, NA, NA,
                    "Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions."),
    defineParameter("studyAreaName", "character", "NT1", NA, NA,
                    "One of 'NT1' or 'NT1_BCR6'.")
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch", objectClass = "RasterLayer",
                 desc = "template raster", sourceURL = NA),
    expectsInput("rasterToMatchLarge", objectClass = "RasterLayer",
                 desc = "template raster for larger area", sourceURL = NA),
    expectsInput("rasterToMatchReporting", objectClass = "RasterLayer",
                 desc = "template raster for reporting area", sourceURL = NA),
    expectsInput("studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area used for simulation (buffered to mitigate edge effects)",
                 sourceURL = NA),
    expectsInput("studyAreaLarge", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area used for module parameterization (buffered)",
                 sourceURL = NA),
    expectsInput("studyAreaReporting", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area used for reporting/post-processing",
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("sppColorVect", objectClass = "character",
                  desc = "species colours for plotting"),
    createsOutput("sppEquiv", objectClass = "data.table",
                  desc = "table of LandR species names equivalencies"),
    createsOutput("sppEquivCol", objectClass = "character",
                  desc = "name of column to use in sppEquiv"),
    createsOutput("studyAreaPSP", objectClass = "SpatialPolygonsDataFrame",
                  desc = paste("this area will be used to subset PSP plots before building the statistical model.",
                               "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                               "Alberta, and Boreal British Columbia"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.caribouNT_studyArea = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "caribouNT_studyArea", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "caribouNT_studyArea", "save")
    },
    warning(paste("Undefined event type: \"", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))

  sim$sppEquiv <- makeSppEquivWBI(studyAreaName)
  sim$sppEquivCol <- "LandR"

  ## studyArea-specific shapefiles and rasters
  allowedStudyAreas <- c("NT1")

  sim$studyArea$studyAreaName <- P(sim)$studyAreaName  # makes it a data.frame

  ## Paired handles 12 colours so it is safer compared to Accent's 8 max
  sim$sppColorVect <- LandR::sppColors(sppEquiv = sim$sppEquiv, sppEquivCol = sim$sppEquivCol,
                                       palette = "Paired", newVals = "Mixed")

  stopifnot(getOption("reproducible.useNewDigestAlgorithm") == 2)

  ## only use ecozones in the WBI study area
  ecozonesToUse <- c(
    "Boreal Cordillera", "Boreal PLain", "Boreal Shield", "Hudson Plain",
    "Southern Arctic", "Taiga Cordillera", "Taiga Plain", "Taiga Shield"
  )

  ## ecozone boundaries within WBI
  ecozones_WB <- Cache(prepInputs,
                       targetFile = "ecozones.shp",
                       archive = asPath("ecozone_shp.zip"),
                       url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                       alsoExtract = "similar",
                       destinationPath = dPath,
                       filename2 = NULL,
                       studyArea = mod$WBstudyArea,
                       overwrite = TRUE,
                       useSAcrs = TRUE,
                       fun = "sf::st_read",
                       userTags = c("prepInputsEcozones", currentModule(sim), cacheTags))
  ecozones_WB <- ecozones_WB[ecozones_WB$ZONE_NAME %in% ecozonesToUse, ] ## remove boundary artifacts

  ## ecozone boundaries current study area
  ecozones_SA <- Cache(prepInputs,
                       targetFile = "ecozones.shp",
                       archive = asPath("ecozone_shp.zip"),
                       url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                       alsoExtract = "similar",
                       destinationPath = dPath,
                       filename2 = NULL,
                       studyArea = sim$studyAreaReporting,
                       overwrite = TRUE,
                       useSAcrs = TRUE,
                       fun = "sf::st_read",
                       userTags = c("prepInputsEcozones", currentModule(sim), cacheTags))
  ecozones_SA <- ecozones_SA[ecozones_SA$ZONE_NAME %in% ecozonesToUse, ] ## remove boundary artifacts

  ## use ecozone boundaries within WBI study area for parameterizing PSP for current study area
  sim$studyAreaPSP <- ecozones_WB[ecozones_WB$ZONE_NAME %in% ecozones_SA$ZONE_NAME, ] %>% sf::as_Spatial()

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  mod$studyAreaNameLong <- "Northwest Territories & Nunavut" ## used for climate data

  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  needStudyArea <- !suppliedElsewhere("studyArea", sim)

  if (isTRUE(needStudyArea)) {
    #### Prep study-area specific objects ####
    ## when adding study areas, add relevant climate urls, rtm and sa, and don't forget R script prepSppEquiv

    fn1 <- function(x) {
      x <- readRDS(x)
      x <- st_as_sf(x)
      st_transform(x, mod$targetCRS)
    }
    canProvs <- Cache(prepInputs,
                      "GADM",
                      #fun = "base::readRDS",
                      fun = fn1,
                      dlFun = "raster::getData",
                      country = "CAN", level = 1, path = dPath,
                      #targetCRS = mod$targetCRS, ## TODO: fails on Windows
                      targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                      destinationPath = dPath,
                      useCache = P(sim)$.useCache) #%>%
    nwt <- canProvs[canProvs$NAME_1 %in% "Northwest Territories", ]

    if (P(sim)$studyAreaName == "NT1") {
      ## NT1 caribou management area
      sim$studyArea <- Cache(
        prepInputs,
        url = "https://drive.google.com/file/d/1AOfJmIzZqQvQWwC7fJWEkmXCj3y6uwWF/",
        targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
        alsoExtract = "similar",
        destinationPath = dPath,
        omitArgs = "destinationPath"
      )
    } else if (P(sim)$studyAreaName == "NT1_BCR6") {
      ## NT1 x BCR 6 intersection
      sim$studyArea <- Cache(
        prepInputs,
        url = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/",
        destinationPath = dPath,
        omitArgs = "destinationPath"
      )
    }
    sim$studyArea <- sim$studyArea %>%
      st_transform(mod$targetCRS) %>%
      st_union(by_feature = TRUE) %>%
      as_Spatial(.) %>%
      raster::aggregate() %>%
      spatialEco::remove.holes()
    ## TODO: do we need to crop this to NT admin boundaries? small part extends into YT
  }

  if (!suppliedElsewhere("studyAreaReporting", sim)) {
    sim$studyAreaReporting <- sim$studyArea
  }

  if (isTRUE(needStudyArea)) {
    ## NOTE: studyArea and studyAreaLarge are the same [buffered] area
    sim$studyArea <- st_as_sf(sim$studyArea) %>%
      st_convex_hull() %>%
      st_buffer(P(sim)$bufferDist) %>%
      as_Spatial()
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    sim$studyAreaLarge <- sim$studyArea
  }

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                               year = 2005,
                               studyArea = sim$studyArea,
                               destinationPath = dPath,
                               useCache = P(sim)$.useCache,
                               overwrite = TRUE,
                               filename2 = paste0(P(sim)$studyAreaName, "_rtm.tif"))
    #sim$rasterToMatch[] <- sim$rasterToMatch[] ## bring raster to memory
  }

  if (!suppliedElsewhere("rasterToMatchLarge", sim)) {
    sim$rasterToMatchLarge <- sim$rasterToMatch
  }

  if (!suppliedElsewhere("rasterToMatchReporting", sim)) {
    # This was raster::mask -- but that sometimes doesn't work because of incorrect dispatch that
    #  conflicts with devtools::load_all("reproducible")
    sim$rasterToMatchReporting <- Cache(maskInputs, sim$rasterToMatch, sim$studyAreaReporting)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
