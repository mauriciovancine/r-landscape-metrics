
# link ----
#https://www.alexchubaty.com/grainscape/articles/grainscape_vignette.html

# packages ----
install.packages("grainscape")
library(grainscape)
library(raster)
library(knitr)

# Step 1: Preparing the resistance surface ----
patchy <- raster(system.file("extdata/patchy.asc", package = "grainscape"))
patchy

## Create an is-becomes matrix for reclassification
isBecomes <- cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6))
patchyCost <- reclassify(patchy, rcl = isBecomes)

## Plot this raster using ggplot2 functionality
## and the default grainscape theme 
ggplot() +
  geom_raster(data = ggGS(patchyCost),
              aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(palette = "Paired", guide = "legend") +
  guides(fill = guide_legend(title = "Resistance")) +
  theme(legend.position = "right")

# Step 2: Extracting the MPG ----
patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))

# Step 3: Quick visualization of the MPG ----
plot(patchyMPG, quick = "mpgPlot", theme = FALSE)

# Step 4: Reporting on the MPG ----
## Extract tabular node information using the graphdf() function
nodeTable <- graphdf(patchyMPG)[[1]]$v

## Render table using the kable function,
## retaining the first three rows
kable(nodeTable[1:3, ], digits = 0, row.names = FALSE)

## Extract tabular link information using the graphdf() function
linkTable <- graphdf(patchyMPG)[[1]]$e

## Render table using the kable function,
## retaining the first three rows
kable(linkTable[1:3, ], digits = 0, row.names = FALSE)

# Step 5: Thresholding the MPG ----
scalarAnalysis <- threshold(patchyMPG, nThresh = 5)

## Use kable to render this as a table
kable(scalarAnalysis$summary,
      caption = paste("The number of components ('nComponents') in the",
                      "minimum planar graph at five automatically-selected",
                      "link thresholds ('maxLink)."))

scalarAnalysis <- threshold(patchyMPG, nThresh = 100)
ggplot(scalarAnalysis$summary, aes(x = maxLink, y = nComponents)) +
  geom_line(colour = "forestgreen") +
  xlab("Link Threshold (resistance units)") +
  ylab("Number of components") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  scale_y_continuous(breaks = 1:20) +
  theme_light() + theme(axis.title = element_text())

# Step 6: Visualizing a thresholded graph ----
