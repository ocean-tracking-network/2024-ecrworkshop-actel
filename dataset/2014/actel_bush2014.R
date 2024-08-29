library("actel")

water <- shapeToRaster(shape = "../bush_study_area_for_distances_epsg29902.shp", size = 5,
					   coord.x = "x.29902", coord.y = "y.29902")
tl <- transitionLayer(water)
distancesMatrix(tl, coord.x = "x.29902", coord.y = "y.29902")
y # save distance matrix as a distances.csv file

bush14 <- migration(tz = "Europe/London", report = TRUE)
y # save strays?
y # save results?

save(tl, file = "../bush_transition_layer.RData")