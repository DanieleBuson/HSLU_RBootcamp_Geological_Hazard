1. Explanation of the folders. 

folder: data
content: 
	usgs_current (not used in the actual code)
	usgs_main (starting dataset of the project)
	volcano_eruptions (used in the analysis of risk)

folder: images
content: 
	Daniele.jpg (image)
	Morty.jpg (image)
	
folder: project_script
content: 
	.RData 
	.Rhistory
	project_script_earthquake.R (file containing all the code that was done in the project)
	
folder: rmarkdown
content: 
	folder: USGSEarthquakes_cache
	folder: USGSEarthquakes_doc_files
	folder: USGSEarthquakes_files
	.Rhistory
	USGSEarthquakes.html (HTML document, complete report on Earthquakes and Volcanoes)
	USGSEarthquakes.rmd (rmd script)

folder: shiny_app
content: 
	earthquake_shiny.R (file r containing the shiny app)

2. Suggestions. 
	1. You need to fix the path, ours are made in windows. Here is a list of lines of code that must be changed if you want to run the code locally.
		- in project_script/project_script_earthquake.r: 
			* line 12 -> earthquake <- read.csv("YOUR PATH/usgs_main.csv")
			* line 355 -> volcano <- read.csv("YOUR PATH/volcano_eruptions.csv",
                      						header = TRUE)
		- in rmarkdown/USGSEarthquakes.rmd:
			* line 22 -> earthquake <- read.csv("YOUR PATH/usgs_main.csv")
			* line 322 -> volcano <- read.csv("YOUR PATH/volcano_eruptions.csv",
                      						header = TRUE)
		- in shiny_app/earthquake_shiny.r: 
			* line 1 -> earthquake <- read.csv("YOUR PATH/usgs_main.csv")
			* line 180 -> output$Daniele <- renderImage({ list(src = "YOUR PATH/Daniele.jpg", width="25%")}, deleteFile = F)
			* line 181 -> output$Morty <- renderImage({ list(src = "YOUR PATH/Morty.jpg", width="25%")}, deleteFile = F)
	
	2. please install the following libraries: 
		- "ggplot2", "plotly", "sf", "rnaturalearth", "rnaturalearthdata", "dplyr", "tidyverse", "shiny", "shinythemes"
	