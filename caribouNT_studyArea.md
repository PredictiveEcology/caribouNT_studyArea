---
title: "caribouNT_studyArea"
author: ""
date: "23 March 2022"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Create study area objects required for running LandR-fireSense simulations for caribou management in the Northwest Territories, Canada.

# Parameters

Provide a summary of user-visible parameters.


|paramName        |paramClass |default |min |max |paramDesc                                                                                                                                                |cacheKey |
|:----------------|:----------|:-------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|
|.plotInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                                |TRUE     |
|.plotInterval    |numeric    |NA      |NA  |NA  |Describes the simulation time interval between plot events.                                                                                              |TRUE     |
|.saveInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                                |TRUE     |
|.saveInterval    |numeric    |NA      |NA  |NA  |This describes the simulation time interval between save events.                                                                                         |TRUE     |
|.useCache        |logical    |FALSE   |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |TRUE     |
|bufferDist       |numeric    |20000   |NA  |NA  |Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions.                                                                       |TRUE     |
|studyAreaName    |character  |NT1     |NA  |NA  |One of 'NT1' or 'NT1_BCR6'.                                                                                                                              |TRUE     |

# Events

Currently everything happens in a single `init` event.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("caribouNT_studyArea", "..")` may be sufficient.


|objectName             |objectClass              |desc                                                               |sourceURL |
|:----------------------|:------------------------|:------------------------------------------------------------------|:---------|
|rasterToMatch          |RasterLayer              |template raster                                                    |NA        |
|rasterToMatchLarge     |RasterLayer              |template raster for larger area                                    |NA        |
|rasterToMatchReporting |RasterLayer              |template raster for reporting area                                 |NA        |
|studyArea              |SpatialPolygonsDataFrame |study area used for simulation (buffered to mitigate edge effects) |NA        |
|studyAreaLarge         |SpatialPolygonsDataFrame |study area used for module parameterization (buffered)             |NA        |
|studyAreaReporting     |SpatialPolygonsDataFrame |study area used for reporting/post-processing                      |NA        |

## Output data

Description of the module outputs.


|objectName   |objectClass              |desc                                                                                                                                                                                            |
|:------------|:------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|sppColorVect |character                |species colours for plotting                                                                                                                                                                    |
|sppEquiv     |data.table               |table of LandR species names equivalencies                                                                                                                                                      |
|sppEquivCol  |character                |name of column to use in sppEquiv                                                                                                                                                               |
|studyAreaPSP |SpatialPolygonsDataFrame |this area will be used to subset PSP plots before building the statistical model. Currently PSP datasets with repeat measures exist only for Saskatchewan, Alberta, and Boreal British Columbia |

# Links to other modules

Initially developed for use with LandR Biomass forest dynamics and fireSense wildfires suites of modules.
