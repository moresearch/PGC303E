#!/usr/bin/Rscript --vanilla
args = commandArgs(trailingOnly=TRUE)

#install.packages(c("rsvg"), repos = "http://cran.us.r-project.org")
#install.packages(c("ggplot2", 
				   #"devtools", 
				   #"tidyverse", 
#				   "readxl", 
#				   "ggthemes"),repos = "http://cran.us.r-project.org")
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
#data
#data_long

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 20){

	# Requires magick R Package https://github.com/ropensci/magick

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
		#image_annotate("http://isel.ufu.br/", color = "white", size = 30, location = "+10+50", gravity = "northeast")
		#logo <- magick::image_background("grey", flatten = TRUE) 
	#logo <- magick::image_border("grey", "600x10") 
	#logo <- magick::image_annotate("Powered By R", color = "white", size = 30, location = "+10+50", gravity = "northeast")
	#####

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
	#print(newdata[project, input])
	#my<-paste(project,newdata[input,project])
	plot<- newdata%>%
		ggplot(aes(domain, !!sym(input)))+
		#ggplot(aes(x=domain, y = total.files.changed, color = domain))+
		#ggplot(aes(x=domain, y = factor(col), color = domain))+
		#ggplot(aes(x=domain, y=input, color=domain))+
		geom_boxplot()+ # are we sure to igonre outliers?
		labs(title = "",subtitle = "", caption = "", x = "domain",y = input) +
			theme_minimal() +
			theme(text=element_text(size = 8))+

			#theme(axis.title.x=element_blank(),
			#	  axis.text.x=element_blank(),
			#	  axis.ticks.x=element_blank())+
			#coord_cartesian(ylim = c(100, 300))+ # maybe fix bad look
			#scale_y_continuous(limits = c(10, 50))+
			#scale_fill_brewer(palette="Dark2")+
			#scale_x_log10()+
			geom_jitter(aes(color=project)) #+
		#coord_flip()
}

denplot <- function(input){
	newdata<-arrange(data, !!sym(input)) 
	plot <- data%>%
		ggplot(aes(x=!!sym(input), fill=domain)) +
		geom_density(alpha=0.3)+
		scale_x_log10()+
		#labs(title = "Github Topic-based Data Analysis",subtitle = "Mohamed, coursename Prof Courscode: , Prof. , 2022", caption = "isel.ufu.br", x = "domains",y = "total lines") +
		theme_minimal() +
		theme(text=element_text(size = 8)) #+
	#geom_boxplot(outlier.alpha = 0.9)
	#geom_histogram(aes(y=..density..), colour="black", fill="white")+
	# geom_density(alpha=.2, fill="#FF6666") 
	#geom_histogram()
	#geom_boxplot()
	#geom_bar()
	#geom_density()
	#geom_point()
}

plotpage <- function(input){
	#input <- !!sym(input) 
	#ggsave(filename="Rplots.pdf", 
	#TODO use paste or somthing to concatnate.
	path<- paste("FIG/",input,".png",sep="")
	ggsave(filename=path, 
	#ggsave(filename="FIG/page1.png", 
		   #plot = (p1 | p1 | p1 | p1 | p1 | p1 | p1 | p1 | p1 | p1 | p1 ) / p2,
		   #plot = ( pie(topics) | p2  / (code_boxplot("total.files.changed") |
		   #plot =  (code_boxplot("total.files.changed") |

		   #plot =  p2  / (code_boxplot("total.files.changed") |
		   #				  code_boxplot("total.lines.added") |
		   #				  code_boxplot("total.lines.deleted") |
		   ##				  code_boxplot("total.lines")),#|


		   #code_denplot <- function(input){
		   #plot=(denplot("total.files.changed") / boxplot("total.files.changed")) |
		   #	   (denplot("total.lines") / boxplot("total.lines")) |
		   #	   (denplot("total.lines.added") / boxplot("total.lines.added")) | 
		   #	   (denplot("total.lines.deleted") / boxplot("total.lines.deleted")),

		   plot=(denplot(input) / boxplot(input)), 

		   #code_boxplot("lines.deleted.added.ratio"))
		   #plot = (p2),
		   #device = cairo_pdf, 
		   device = png, 
		   #width = 497, 
		   #height = 210, 
		   units = "mm")


	#path<- paste("FIG/",input,".png",sep="")
	#path_logo<- paste("FIG/",input,".logo.png",sep="")
	#ggsave(filename=path, 
	plot_with_logo <- add_logo(
							   #plot_path = "FIG/page1.png", # url or local file for the plot
							   plot_path = path, # url or local file for the plot
							   logo_path = "FIG/logo.jpg", # url or local file for the logo
							   logo_position = "top right", # choose a corner
							   # 'top left', 'top right', 'bottom left' or 'bottom right'
							   #logo_scale = 10 as default, but can change to manually make logo bigger
							   logo_scale = 10 
	)
	# save the image and write to working directory
	#magick::image_write(plot_with_logo, "plot_with_logo.png")
	#magick::image_write(plot_with_logo, "FIG/plot_with_logo.png")
	#magick::image_write(plot_with_logo, path_logo)
	magick::image_write(plot_with_logo, path)
}

# "domain", "project", "total.files.changed", "total.lines.added", "total.lines.deleted", "total.lines", "lines.deleted.added.ratio"
plotpage("total.files.changed") 
plotpage("total.lines.added") 
plotpage("total.lines.deleted") 
plotpage("total.lines") 
#plotpage("total.files.changed") 

#logo <- image_read("FIG/logo.jpg") %>% image_resize("100x100") 
#myplot <- image_read("FIG/page1.png")
#out <- image_composite(myplot, logo, offset = "+70+30") 
#magick::image_write(out, "FIG/plot_with_logo.png")




#geom_jitter(aes(color=domain))+
# coord_cartesian(ylim = c(0, 300))+ # maybe fix bad look

#scale_fill_manual(values = c("#0099f8", "#e74c3c", "#2ecc71")) +
#stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9) +
#stat_summary(fun = "mean", geom = "point", shape = 2, size = 4) +
#geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
#facet_grid()+
#facet_grid(cols =  vars(variable)) +
#facet_grid(rows = vars(value), cols =  vars(variable))
#facet_wrap(vars(variable)) +
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#999999","Red", "green","yellow"))
#stat_summary(fun = "mean", geom = "point", shape = 8, size = 2, color = "white")


