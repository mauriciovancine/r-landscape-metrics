# packages ----
library(devtools)
library(remotes)
install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")

library(Makurhini)

# Protected Connected Land (ProtConn) ----
test_protconn <- Makurhini::MK_ProtConnMult(nodes = Protected_areas, 
                                            region = ecoregions,
                                            attribute = "Intersected area",
                                            area_unit = "ha",
                                            distance = list(type= "centroid"),
                                            distance_thresholds = 10000,
                                            probability = 0.5, 
                                            transboundary = 50000,
                                            plot = TRUE, 
                                            CI = NULL,
                                            parallel = TRUE, 
                                            intern = FALSE)
test_protconn[[1]][[1]]

# Equivalent Connectivity Area (ECA) ----

data("list_forest_patches", package = "Makurhini")
class(list_forest_patches)

data("study_area", package = "Makurhini")
class(study_area)[1]

dECA_test <- MK_dECA(nodes= list_forest_patches, 
                     attribute = NULL, 
                     area_unit = "ha",
                     distance = list(type= "centroid"), 
                     metric = "PC",
                     probability = 0.05, 
                     distance_thresholds = 5000,
                     LA = Max_attribute,
                     plot= c("1993", "2003", "2007", "2011"))
dECA_test

Max_attribute <- unit_convert(gArea(study_area), "m2", "ha")

# Integral index of connectivity (IIC) and fractions (Intra, Flux and Connector) ----

data("vegetation_patches", package = "Makurhini")
nrow(vegetation_patches) # Number of patches
class(vegetation_patches)[1]

IIC <- MK_dPCIIC(nodes = vegetation_patches, 
                 attribute = NULL,
                 distance = list(type = "centroid"),
                 metric = "IIC", 
                 distance_thresholds = 10000) #10 km
head(IIC)

# Probability of connectivity (PC) and fractions (Intra, Flux and Connector) ----

PC <- MK_dPCIIC(nodes = vegetation_patches, 
                attribute = NULL,
                distance = list(type = "centroid"),
                metric = "PC", 
                probability = 0.05,
                distance_thresholds = 10000)
head(PC)


# Centrality measures ----

centrality_test <- MK_RMCentrality(nodes = vegetation_patches,
                                   distance = list(type = "centroid"),
                                   distance_thresholds = 10000,
                                   probability = 0.05,
                                   write = NULL)
head(centrality_test)
