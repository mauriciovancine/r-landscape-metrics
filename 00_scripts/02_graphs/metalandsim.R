# link
# https://www.r-bloggers.com/2019/02/individual-patch-connectivity/

install.packages("MetaLandSim", dep = TRUE)
library(MetaLandSim)

set.seed(123)

rl1 <- rland.graph(mapsize=500, dist_m=50, areaM=0.1, 
                   areaSD=0.02, Npatch=50,
                   disp=70, plotG=TRUE)

#as.numeric is just to guarantee the output is just a number
full.IIC <- as.numeric(metrics.graph (rl=rl1, metric="IIC"))

patches <- rl1$nodes.characteristics

#Here are the top 6 rows:
head(patches)

dIIC <- rep(NA, 50)

#the new version of the function:
removepoints.byID <- function (rl, nr=1, ID){
    
    if (class(rl)!="landscape") 
    {
        stop(paste(rl, " should be an object of class class 'landscape'.", sep=""), call. = FALSE)
    } 
    mapsize2 <- rl$mapsize
    dist_m2 <- rl$minimum.distance
    areaM2 <- rl$mean.area
    areaSD2 <- rl$SD.area
    Npatch2 <- rl$number.patches
    disp2 <- rl$dispersal
    rl_0 <- rl$nodes.characteristics
    ID2 <- rl_0$ID
    nr_select <- nrow(rl_0)-nr
    rl_1 <- rl_0[-ID, ]
    rl_2 <- rl_1[sort.list(as.numeric(rownames(rl_1))), ]
    names(rl_2)[names(rl_2) == "ID2"] <- "ID"
    rl_3 <- list(mapsize=mapsize2, minimum.distance=dist_m2,
                 mean.area=mean(rl_2$areas), SD.area=sd(rl_2$areas),
                 number.patches=nrow(rl_2), dispersal=disp2,
                 nodes.characteristics=rl_2)
    class(rl_3) <- "landscape"
    rl_4 <- cluster.id(rl_3)
    rownames(rl_4$nodes.characteristics) <- 1:nrow(rl_4$nodes.characteristics)
    class(rl_4) <- "landscape"
    return(rl_4)
}

#Set up the loop to do the calculations
for (i in 1:50){
    
    rl2 <- rl1 #This is just no to change rl1
    
    rl3 <- removepoints.byID(rl1,ID=i)#removing patch i
    
    partial.IIC <- as.numeric(metrics.graph (rl=rl3, metric="IIC"))
    
    dIIC[i] <- 100*((full.IIC-partial.IIC)/full.IIC)#send the result to the vector
    
}

#And here's the result:
dIIC

plot_graph(rl=rl1, species=FALSE, links=TRUE)

#The individual connectivity patches
text(x = patches[,'x'],y = patches[,'y'], pos = 3, 
     offset = 0.2, labels = round(dIIC,2))
