rm(list = ls())

library(lattice)
library(latticeExtra)
library(maptools)
library(raster)
library(RColorBrewer)
library(tidyverse)


# read data --------------------------------------------------------------------
data_list <- lapply(
	FUN = function(i) {
		res <- read.table(file = paste0(i, "_data.csv"), 
			stringsAsFactors = FALSE, header = FALSE)
		return(res)
	}, X = c("x", "y", "z")
)
names(data_list) <- c("x", "y", "z")


# fix paramates for vizualization ----------------------------------------------
wrld <- maps::map('world', xlim = c(-180, 180),
	ylim = c(-90, 90))
wrld <- data.frame(lon = wrld$x, lat = wrld$y)

mycols <- brewer.pal(9, "RdYlBu")[c(-2, -4:-7)]
mybreaks <- c(-2, -0.5, 0, 0.5, 1.5)

# data tarnsformation ----------------------------------------------------------
coord_mx <- t(data_list[["z"]])

df <- as.data.frame(coord_mx, stringsAsFactors = FALSE)
names(df) <- paste0("cn", seq(along.with = df[1, ]))
colnames(df) <- paste0("x_", unlist(data_list[["x"]]))
df[, "y"] <- data_list[["y"]]
df_long <- df %>% pivot_longer(cols = starts_with("x"),
	names_to = "x", values_to = "z")

df_long[, "x"] <- as.numeric(gsub(df_long$x, 
	pattern = "x_", replacement = ""))

data(wrld_simpl)

# note that x and y are reversed
pts <- SpatialPoints(df_long[, c("y", "x")], 
	proj4string=CRS(proj4string(wrld_simpl)))

# find which points fall over land
ii <- !is.na(over(pts, wrld_simpl)$FIPS)
df_long_clean_ocean <- df_long[ii, ]

# visualization ----------------------------------------------------------------

# filled.contour() -> filledContour()
pdf("raster_filledCont.pdf")
filledContour(rasterFromXYZ(df_long_clean_ocean), nlevels = 7,
	col = mycols)
dev.off()

pdf("raster_filledCont_with_map.pdf")
filledContour(rasterFromXYZ(df_long_clean_ocean), nlevels = 7,
	col = mycols)
maps::map("world", add=TRUE)
dev.off()	

# ggplot2
map.world <- map_data("world")

ggplot(data = df_long_clean_ocean,
	aes(x = y, y = x, z = z, fill = z)) +
	geom_tile() +
	scale_fill_fermenter(breaks = mybreaks,
		palette = "RdYlBu", direction = 1) +
	geom_map(dat=map.world, map = map.world, aes(map_id=region), 
	           fill="NA", color="black", inherit.aes = F) +
	coord_quickmap() +
	labs(x = "long", y = "lat", 
	fill = "",
	title = "", 
	subtitle = paste0(""),
	inherit.aes = FALSE) +
	theme(panel.border = element_rect(colour = "gray10", 
	fill = NA, size = 0.1),
	legend.text = element_text(size = 14),
	axis.text = element_text(size = 14))
ggsave("ggplot_tile.pdf")


ggplot(data = df_long_clean_ocean,
	aes(x = y, y = x, z = z)) +
	geom_contour_filled(breaks = mybreaks, 
		# col = "black", 
		size = 0.01) +
	scale_fill_brewer(palette = "RdYlBu") +
	geom_map(dat=map.world, map = map.world, aes(map_id=region), 
	           fill="NA", color="black", inherit.aes = F) +
	coord_quickmap() +
	labs(x = "long", y = "lat", 
	fill = "",
	title = "", 
	subtitle = paste0(""),
	inherit.aes = FALSE) +
	theme(panel.border = element_rect(colour = "gray30", 
	fill = NA, size = 0.1),
	legend.text = element_text(size = 14),
	axis.text = element_text(size = 14))
ggsave("ggplot_contour_fill_geo.pdf")	

ggplot(data = df_long_clean_ocean,
	aes(x = y, y = x, z = z)) +
	geom_contour_filled(breaks = c(-2, -0.5, -0.01, 0, 0.01, 0.5, 1.5), 
		# col = "black", 
		size = 0.01) +
	scale_fill_brewer(palette = "RdYlBu") +
	geom_map(dat=map.world, map = map.world, aes(map_id=region), 
	           fill="NA", color="black", inherit.aes = F) +
	coord_quickmap() +
	labs(x = "long", y = "lat", 
	fill = "",
	title = "", 
	subtitle = paste0(""),
	inherit.aes = FALSE) +
	theme(panel.border = element_rect(colour = "gray30", 
	fill = NA, size = 0.1),
	legend.text = element_text(size = 14),
	axis.text = element_text(size = 14))
ggsave("ggplot_contour_fill2_geo.pdf")	

# lattice
lp <- levelplot(data = df_long_clean_ocean, 
				z ~ y + x, 	
				pretty = TRUE, 
				at = mybreaks,
				aspect = 1.25, 
				col.regions = mycols, 
				contour = TRUE,
				# margin = TRUE,
				xlab = "", ylab = "")

wp <- xyplot(
	data = wrld, lat ~ lon, 
	lty = 1, lwd = 0.5, fill = 'black', col = 'black', pch = 21,
	type = c("l", "g"), 
	# type = c("l"), 
	cex = 0.5,
	# margin = TRUE,
	panel = function(...) {
		panel.xyplot(...)
		panel.grid(h = -1, v = -1, col.line = "gray10", lwd = 0.5)
	}
	)

res <- lp + as.layer(wp)

pdf("lattice_cp.pdf")
	plot(res)
dev.off()
