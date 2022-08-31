#!/usr/bin/Rscript --vanilla
args = commandArgs(trailingOnly=TRUE)

suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(readxl))
suppressMessages(library(ggthemes))
suppressMessages(library(reshape2))
suppressMessages(library(patchwork))
suppressMessages(library(magick)) # sudo apt-get install libmagick++-dev and require(installr)
#suppressMessages(library(dplyr))
#suppressMessages(library(svglite))
#suppressMessages(library(ggpattern))
#suppressMessages(library(lubridate))
#suppressMessages(library(fmsb))
#suppressMessages(library(scales))
#suppressMessages(library(geosphere))
#suppressMessages(library(d3radarR))
#suppressMessages(library(RColorBrewer))
#suppressMessages(library(scales))

#devtools::install_github("timelyportfolio/d3radarR")
#suppressMessages(library(RColorBrewer))
#suppressMessages(library(ggradar))

setwd(".") # Set Working Dir  

print(args[1])
mycsv<- args[1] 
data<-read.csv(file.path(normalizePath(dirname(mycsv)),basename(mycsv)), header=TRUE)
data<-na.omit(data)
data = subset(data, select = -c(lines.deleted.added.ratio))
data_long <- melt(data)# Reshaping data frame

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 20){

	# Useful error message for logo position
	if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
		stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
	}

    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)

    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width

    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))

	#####

	logo <- logo %>%
		image_scale("100%") %>% 
		image_background("white", flatten = TRUE) %>%
		image_border("white", "200x10") #%>%

    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height

    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding

    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }

    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))

}

boxplot <- function(input){
	newdata<-arrange(data, !!sym(input)) 
	plot<- newdata%>%
		ggplot(aes(domain, !!sym(input)))+
		geom_boxplot()+ # are we sure to igonre outliers?
		labs(title = "",subtitle = "", caption = "", x = "domain",y = input) +
			theme_minimal() +
			theme(text=element_text(size = 8))+
			geom_jitter(aes(color=project)) 
}

denplot <- function(input){
	newdata<-arrange(data, !!sym(input)) 
	plot <- data%>%
		ggplot(aes(x=!!sym(input), fill=domain)) +
		geom_density(alpha=0.3)+
		scale_x_log10()+
		theme_minimal() +
		theme(text=element_text(size = 8)) 
}

plotpage <- function(input){
	path<- paste("FIG/",input,".png",sep="")
	ggsave(filename=path, 
		   plot=(denplot(input) / boxplot(input)), 
		   device = png, 
		   units = "mm")


	plot_with_logo <- add_logo(
							   plot_path = path, # url or local file for the plot
							   logo_path = "FIG/logo.jpg", # url or local file for the logo
							   logo_position = "top right", # choose a corner
							   logo_scale = 10 
	)
	magick::image_write(plot_with_logo, path)
}

# "domain", "project", "total.files.changed", "total.lines.added", "total.lines.deleted", "total.lines", "lines.deleted.added.ratio"
plotpage("total.files.changed") 
plotpage("total.lines.added") 
plotpage("total.lines.deleted") 
plotpage("total.lines") 
