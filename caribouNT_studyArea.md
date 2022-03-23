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


|paramName        |paramClass |default |min |max |paramDesc                                                                                                                                                |
|:----------------|:----------|:-------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------------------------------------|
|.plotInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                                |
|.plotInterval    |numeric    |NA      |NA  |NA  |Describes the simulation time interval between plot events.                                                                                              |
|.saveInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                                |
|.saveInterval    |numeric    |NA      |NA  |NA  |This describes the simulation time interval between save events.                                                                                         |
|.useCache        |logical    |FALSE   |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant |
|bufferDist       |numeric    |20000   |NA  |NA  |Distance (m) to buffer studyArea and rasterToMatch when creating 'Large' versions.                                                                       |
|studyAreaName    |character  |RIA     |NA  |NA  |One of 'AB', 'BC', 'MB', 'NT', 'SK', 'YT', or 'RIA'.                                                                                                     |

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



# Links to other modules

Initially developed for use with LandR Biomass forest dynamics and fireSense wildfires suites of modules.
