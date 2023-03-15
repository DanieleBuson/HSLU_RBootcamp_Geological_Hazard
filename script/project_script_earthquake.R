################################################################################
####################### USGS EARTHQUAKE DATASET ################################
################################################################################
# Earthquakes form an integral part of our planet’s geology. 
# It is crucial to gain an understanding of the frequency and strength of
# these seismic activities, as this information is essential in both the cause 
# and prevention of damaging earthquakes. Fortunately for us, 
# the United States Geological Survey (USGS) captures comprehensive data on 
# Earthquakes magnitude and location across the United States and 
# its surrounding areas.

earthquake <- read.csv("C:/Users/buson/OneDrive/Desktop/r_project/data/usgs_main.csv")
# head(earthquake)
earthquake <- earthquake[!is.na(earthquake$mag),]

earthquake$time <- as.POSIXct(earthquake$time, tz = "", format = "%Y-%m-%d")
days <- format(earthquake$time, "%d")
months <- format(earthquake$time, "%m")

# Since the time column contains data that are not very useful in their whole, 
# we extract the month and the day from each record, creating two new variables. 
# As a result, we create a new data-set using the command data.frame, called
# "df". From this point on we will work on it.

df <- data.frame(months, 
                 days, 
                 latitude = earthquake$latitude, 
                 longitude = earthquake$longitude, 
                 depth = earthquake$depth, 
                 mag = earthquake$mag, 
                 rms = earthquake$rms, 
                 place = earthquake$place, 
                 type = earthquake$type)

# First of all, we are going to use a subset of the "df" data-set. The main reason
# is the dimension: in fact, more than 75 thousand records can  be difficult
# to handle. In order to avoid the problem, we are going to use the library "dplyr",
# in particular the function "sample_n()" to extract randomly 2500 records.

library(dplyr)

# In the line of code below, you can find a command to replicate the experiment 
# exactly in the same way we did. Of course it is possible to comment it and 
# having a different subset. of course the result will be similar at  
# what we are going to present.
set.seed(1)

df <- sample_n(tbl = df, size = 2500)

################################################################################
####################### DISTRIBUTION PER MONTH #################################
################################################################################

# In this first part our focus is the monthly distribution of records. 
# We therefore create a new data-set using the command "data.frame()" in combination
# with the function "table()" . We store this new data-set with the name 
# "count_months". 
count_months <- data.frame(table(as.numeric(df$months)))

# Here an idea of what it contains. 
head(count_months)

# To show the result we opted for an histogram. The idea is that we want to show the 
# number of records in each month (frequency). For plotting data we decided  to
# use two libraries, "ggplot2" and "plotly". The main graph is created with 
# the function "ggplot()" and stored into a variable "p". Then the plot is performed 
# by the function "ggplotly()".

library(ggplot2)
library(plotly)

p <- ggplot(count_months, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_histogram(stat = "identity") + 
  ggtitle("Distribution of earthquakes per month\n2022 USGS earthquake dataset") +
  xlab("Month") + 
  ylab("Frequency") +
  scale_fill_manual(values = heat.colors(10)) +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

ggplotly(p)

# As we can see from the graph the month with the highest frequency is October.
# However, we cannot say anything more about the data. The minimum frequency is
# registered in the month of December. This is due to the fact that the last
# record date is 12th of December. That means that the month was not complete.
# That explain the low number compared to the other months. We have a similar
# situation with March, since the first day on the record is the 3rd (just 2 days
# missing).
# Our analysis is that the month have no influence on the frequency of earthquakes.
# We want now to analyse if the month has some influence on the magnitude of
# an earthquake.

################################################################################
######################## MAGNITUDE PER MONTH ###################################
################################################################################

# We want to perform an Anova test to evaluate if changing the month affects 
# significantly the magnitude of a earthquake. In other words, we want to measure
# if the intensity of an earthquake is influenced by temporal aspects. 

fit <- aov(mag~months, data = df)
summary(fit)

# The summary result shows that the ANOVA model is significant for a test at 
# significance level alpha = 0.05. In fact the p-value of the test is 0.0192 and 
# the F-statistic is big enough. However, we believe that this statistic is
# due to random circumstances. In fact, changing an arbitrary parameter as the 
# significance level alpha to a lower value (for example 0.01) could be enough 
# to completely change our point of view. We therefore decide not to consider 
# this result in our model. To support our theory we draw some box-plot. 

# Also in this case we use the libraries "ggplot2" and "plotly". In addition, 
# the type of graph that we have selected is the box-plot. The reason is that 
# it let us show the distribution of the values for each month. In particular, 
# with the interactive plot it is possible to visualize median, quartiles and 
# all the outlier. 

p <- ggplot(df, aes(x = months, y = mag, fill = months)) + 
  geom_boxplot(outlier.colour = "black") + 
  ggtitle("Distribution of Magnitude per month
          \n2022 USGS earthquake dataset") +
  xlab("Month") + 
  ylab("Magnitude (in Richter scale)") +
  scale_fill_manual(values = heat.colors(10)) +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

ggplotly(p)

# The next step is purely informative. We are going to print out the information 
# about magnitude for each month. With this purpose we create some data-set. 
# However, we actually print just March and April values, the other are commented. 
# It is possible to easily look at them by selecting them, going into the "Code" 
# menu on top and select "Comment/Uncomment Lines".

march_df <- df[which(as.numeric(df$months)==3), ]
april_df <- df[which(as.numeric(df$months)==4), ]
may_df <- df[which(as.numeric(df$months)==5), ]
june_df <- df[which(as.numeric(df$months)==6), ]
july_df <- df[which(as.numeric(df$months)==7), ]
august_df <- df[which(as.numeric(df$months)==8), ]
september_df <- df[which(as.numeric(df$months)==9), ]
october_df <- df[which(as.numeric(df$months)==10), ]
november_df <- df[which(as.numeric(df$months)==11), ]
december_df <- df[which(as.numeric(df$months)==12), ]

summary(march_df$mag)
summary(april_df$mag)
# summary(may_df$mag)
# summary(june_df$mag)
# summary(july_df$mag)
# summary(august_df$mag)
# summary(september_df$mag)
# summary(october_df$mag)
# summary(november_df$mag)
# summary(december_df$mag)

# After all this considerations, we arrive to the conclusion that months and 
# magnitude are not in a relation of causality. Every seismic event depends on
# the Earth's crust movements. As a result also the magnitude depends on it. 
# Moreover, we want to specify an aspect related to magnitude that will appear
# frequently in our data. The magnitude of an earthquake could be negative. 
# This records are part of the phenomenon of micro-seismicity.This kind of 
# earthquakes are not felt by humans but the devices can detected them. Since their
# intensity (logarithmic) is lower than the 0 in the Richter scale, 
# these values are negative. 

################################################################################
###################### GEOGRAPHICAL DATA VISUALISATION #########################
################################################################################

earthquake <- earthquake[!is.na(earthquake$mag),]

earthquake$time <- as.POSIXct(earthquake$time, tz = "", format = "%Y-%m-%d")

days <- format(earthquake$time, "%d")
months <- format(earthquake$time, "%m")


df_geo_vis <- data.frame(months, days, 
                         latitude = earthquake$latitude, 
                         longitude = earthquake$longitude, 
                         depth = earthquake$depth, 
                         mag = earthquake$mag, 
                         rms = earthquake$rms, 
                         place = earthquake$place, 
                         type = earthquake$type,
                         int_mag= as.integer(earthquake$mag))


country <- world[world$name == "United States of America",]

set.seed(1)

df_geo_vis <- sample_n(tbl = df_geo_vis, size = 5000)

df_geo_vis[, "int_mag"] <- as.character(df_geo_vis[, "int_mag"])

p <-ggplot() + geom_sf(data = world)+ 
  geom_sf(data=country, fill = "blue")+
  
  geom_point(data=df_geo_vis, 
             mapping = aes(x=longitude,
                           y=latitude), 
             color = "red",
             size = 0.3)
  
  # scale_colour_gradientn(colors=rev(rainbow(9)))
  # scale_color_identity()
ggplotly(p)

# This is the first graph we are defining for the geographical representation.  
# Later on, in this report, you can find more deep analysis about it. What we want 
# the reader to understand is the distribution of data. In particular, we want to show
# the most affected areas using, in this case, a standardised colour. However, as it is possible to 
# see from this graphical representation, the majority of the records are in the US
# area. This is a bias that we could not escape and it is linked to the fact that 
# the data are collected from US stations. Our way to avoid the problem was to 
# consider only data with a certain magnitude. 

################################################################################
########################### DEPTH ANALYSIS #####################################
################################################################################

# We want to analyse if there are some interesting patterns in 
# depth distribution. In particular, we are going to investigate if there is 
# some form of correlation between depth and the magnitude of the earthquake.
# We perform the evaluation using "ggplot2" and "plotly".
# The first step is to create data-sets for all the cases 

# In the general case we use just the standard df (2500 records).
p <- ggplot(df, aes(x = depth, fill = type)) +
  geom_histogram() + 
  ggtitle("Distribution of depth") +
  xlab("Seismic event depth") +
  scale_fill_manual(name = "Earthquake type", values = c("#A099FF", "#1B0AFF", "#0C00A8")) +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

# In this part, we analyse the distribution of depth of all the earthquakes that 
# are perceivable. 
df_twopointfive <- df[which(df$mag > 2.5), ]

p25 <- ggplot(df_twopointfive, aes(x = depth, fill = type)) +
  geom_histogram() + 
  ggtitle("Distribution of depth\nMagnitude greater than 2.5") +
  xlab("Seismic event depth") +
  scale_fill_manual(name = "Earthquake type", values = "#E5E600") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

# In this part, we analyse the distribution of depth of all the medium-to-high 
# intensity earthquakes. 
df_four <- df[which(df$mag > 4), ]

p4 <- ggplot(df_four, aes(x = depth, fill = type)) +
  geom_histogram() + 
  ggtitle("Distribution of depth\nMagnitude greater than 4") +
  xlab("Seismic event depth") +
  scale_fill_manual(name = "Earthquake type", values = "#FF7127") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

# In this part, we analyse the distribution of depth of the most intense 
# earthquakes.
df_five <- df[which(df$mag > 5), ]
p5 <- ggplot(df_five, aes(x = depth, fill = type)) +
  geom_histogram() + 
  ggtitle("Distribution of depth\nMagnitude greater than 5") +
  xlab("Seismic event depth") +
  scale_fill_manual(name = "Earthquake type", values = "#FF2200") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

# We are using the library "gridExtra" to create a plot containing
# multiple graphs.
library(gridExtra)
grid.arrange(p, p25, p4, p5, ncol=2, nrow = 2)

# The result show us how the majority of the earthquakes are always in the range 
# between 0 km an 20 km. This could be due to the fact that deeper earthquakes 
# are more difficult to detect by instruments. As a consequence we assume that 
# only intense deep earthquakes can be detected. This hypothesis has to be tested. 
# We are then doing an anova-test between three groups: 
# shallow(0km to 70km), intermediate(70km to 300km), deep(more than 300km)

shallow_eq_df <- earthquake[which(earthquake$depth<70), ]
intermediate_eq_df <- earthquake[which(earthquake$depth>70 & earthquake$depth<300), ]
deep_eq_df <- earthquake[which(earthquake$depth>300), ]

t.test(x = shallow_eq_df$mag, y = intermediate_eq_df$mag, paired = FALSE)
t.test(x = shallow_eq_df$mag, y = deep_eq_df$mag, paired = FALSE)
t.test(x = intermediate_eq_df$mag, y = deep_eq_df$mag, paired = FALSE)
library(dplyr)

set.seed(1)
shallow_eq_df <- sample_n(tbl = shallow_eq_df, size = 300)
intermediate_eq_df <- sample_n(tbl = intermediate_eq_df, size = 300)
deep_eq_df <- sample_n(tbl = deep_eq_df, size = 300)


t.test(x = shallow_eq_df$mag, y = intermediate_eq_df$mag, paired = FALSE)
t.test(x = shallow_eq_df$mag, y = deep_eq_df$mag, paired = FALSE)
t.test(x = intermediate_eq_df$mag, y = deep_eq_df$mag, paired = FALSE)


fit <- aov(shallow_eq_df$mag~deep_eq_df$mag)
summary(fit)
fit <- aov(shallow_eq_df$mag~intermediate_eq_df$mag)
summary(fit)
fit <- aov(intermediate_eq_df$mag~deep_eq_df$mag)
summary(fit)

################################################################################
########################## VOLCANO ACTIVITY ##################################
################################################################################

# In this section, our purpose is to investigate if there is a relation between 
# the volcanic eruptions and earthquakes. As we can see from the books, sometimes 
# the events are correlated. The analysis is focus on finding the geographical 
# areas in which this could be true. The approach will be mainly graphical. 

# First of all, we import the dataset regarding volcano eruptions. 
volcano <- read.csv("C:/Users/buson/OneDrive/Desktop/r_project/data/volcano_eruptions.csv",
                      header = TRUE)
head(volcano)

# We eliminate all the records without a specified position. In addition, we 
# model the volcano data-set, in particular we have changed the value of the 
# "Volcano.Explosivity.Index..VEI." in order to substitute the NA values with 0. 
# We have decided to perform this change because with a zero we can obtain a 
# number that will be used in a second time (in calculating approximately the 
# risk of natural disaster in the area). With Na, we are going to loose 
# information. 
 
volcano <- volcano[which(!is.na(volcano$Latitude | volcano$Longitude)),]
volcano[which(is.na(volcano$Volcano.Explosivity.Index..VEI.)), "Volcano.Explosivity.Index..VEI."] <- 0
volcano[which(is.na(volcano$Volcano.Explosivity.Index..VEI.)),]

# The next step is to create some graphical analysis. In particular we want to 
# show the world distribution of earthquake and volcanic explosions based on the 
# geographical location. The point size depends respectively on the magnitude and 
# the explosiveness index.
# NB: in this section we are using a modified data-set. In fact, we are using 
# only perceivable earthquakes (more than 2.5 magnitude).

# load library tidyverse

library(tidyverse)

# create data for world coordinates using map_data() function

world_coordinates <- map_data("world")

p_v <- ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region), 
    fill = "lightgray", 
    colour = "black") +
  geom_point(data = volcano, 
             aes(x = Longitude, y = Latitude, colour = Volcano.Explosivity.Index..VEI.)
             ) + 
  scale_colour_gradient(name = "Explosiveness Index",low = "#fbf8bc",
                      high = "#FF0000")+
  ggtitle("Historical Volcano Eurptions\nGeographical distribution") +
  theme(
    legend.position = "bottom") +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

p_e <- ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region), 
    fill = "lightgray", 
    colour = "black") + 
  geom_point(data = df_twopointfive, 
             aes(x = longitude, y = latitude, colour = mag)) + 
  scale_colour_gradient(name = "Magnitude",low = "#fbf8bc",
                        high = "#FF0000")+
  ggtitle("Perceivable Earthquakes\nGeographical distribution") +
  theme(
    legend.position = "bottom") +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

# Loading the library "gridExtra" we are going to show all the data into the same 
# panel. 

library(gridExtra)

grid.arrange(p_e, p_v, ncol=2, nrow = 1)

# In addition we want to give a more precise idea of the patterns that are 
# recognizable by creating a new map with all the occurrences.

ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region), 
    fill = "lightgray", 
    colour = "black") + 
  geom_point(data = df_twopointfive, 
             aes(x = longitude, y = latitude), 
             colour = "#BB77FF") +
  geom_point(data = volcano, 
             aes(x = Longitude, y = Latitude), 
             colour = "#f8ec27") +
  ggtitle("Distribution of perceivable earthquakes(violet) and eruptions(yellow)")

# From the map it is possible to see that the area between Japan and Indonesia
# has a strong correspondence. From the geological paper we know that in this 
# area we have the boundary of three geological plates such as Eurasian Plate,
# Philippine Sea Plate and the Australian Plate. Another interesting pattern 
# can be detected in Mexico and South America. Also in this case, the presence 
# of volcanoes and earthquakes seems to be correlated to the presence of tectonic
# plates. Another 2 interesting areas are the south coast of Alaska and in the
# middle of the Atlantic Ocean. 

# We are going to create a grid to show results in a heat-map. 

# In order to do that we first create latitude and longitude vectors. The main 
# purpose is to create the basis for our own grid. We will use also a new column
# called region in order to store a position in the grid for each value. 

latitude = seq(-80.0, 80.0, 5.0)
latitude
longitude = seq(-200.0,200.0, 5.0)
longitude
df_perc_eq <- df_twopointfive
df_perc_eq[,"region"] <- 0

# We create therefore the actual separation using some for loops. We store then 
# the region of each record as an integer number.
# The code could take some seconds. 

i = 0

for (lati in 1:length(latitude)){
  for (longi in 1:length(longitude)){
    i = i + 1
    for (rec in 1:nrow(df_perc_eq)){
      if (df_perc_eq[rec, "latitude"]>latitude[lati] & df_perc_eq[rec, "latitude"]<latitude[lati] + 5){
        if (df_perc_eq[rec, "longitude"]>longitude[longi] & df_perc_eq[rec, "longitude"]<longitude[longi] + 5){
          df_perc_eq[rec, "region"] = i
          rec
        }
      }
    }
  }
}

df_volc <- volcano
df_volc[,"region"] <- 0

i = 0

head(df_volc)
for (lati in 1:length(latitude)){
  for (longi in 1:length(longitude)){
    i = i + 1
    for (rec in 1:nrow(df_volc)){
      if (df_volc[rec, "Latitude"]>latitude[lati] & df_volc[rec, "Latitude"]<latitude[lati] + 5){
        if (df_volc[rec, "Longitude"]>longitude[longi] & df_volc[rec, "Longitude"]<longitude[longi] + 5){
          df_volc[rec, "region"] = i
          rec
        }
      }
    }
  }
}

# Once it is done, we can proceed with the plot of the two heat-maps. The first 
# one will represent the distribution in the world of earthquakes (perceivable 
# ones). The second one will show data about the distribution of historical 
# volcano eruptions. 

ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region),
    fill = "lightgray", 
    colour = "black"
    ) +
  geom_bin2d(data = df_perc_eq, 
             aes(x = longitude, y = latitude)) + 
  scale_fill_gradient(name = "Frequency",low = "#fbf8bc",
                      high = "#FF0000") +
  ggtitle("Distribution of perceivable earthquakes.") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  ) 

ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region),
    fill = "lightgray", 
    colour = "black"
  )  +
  geom_bin2d(data = df_volc,
             aes(x = Longitude, y = Latitude)) +
  scale_fill_gradient(name = "Frequency",low = "#fbf8bc",high = "#FF0000") +
  ggtitle("Distribution of volcano eruptions.") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  ) 



################################################################################
########################### RISK DF ############################################
################################################################################


df_temp_V <- volcano[, c("Latitude", "Longitude")]
df_temp_V[, "Explosivness_Index"] <- as.integer(volcano$Volcano.Explosivity.Index..VEI.)

for (i in 1:length(df_temp_V$Explosivness_Index)){
  if (df_temp_V$Explosivness_Index[i] == 7){
    df_temp_V$Explosivness_Index[i] <- df_temp_V$Explosivness_Index[i]*1.8
  }
  if (df_temp_V$Explosivness_Index[i] == 6){
    df_temp_V$Explosivness_Index[i] <- df_temp_V$Explosivness_Index[i]*1.6
  }
  if (df_temp_V$Explosivness_Index[i] == 5){
    df_temp_V$Explosivness_Index[i] <- df_temp_V$Explosivness_Index[i]*1.4
  }
  if (df_temp_V$Explosivness_Index[i] == 4){
    df_temp_V$Explosivness_Index[i] <- df_temp_V$Explosivness_Index[i]*1.2
  }
  if (df_temp_V$Explosivness_Index[i] <= 2){
    df_temp_V$Explosivness_Index[i] <- df_temp_V$Explosivness_Index[i]*0.8
  }
}
# head(df_temp_V)

df_temp_E <- df[, c("latitude", "longitude")]
df_temp_E[, "mag_int"] <- as.integer(df$mag)

for (i in 1:length(df_temp_E$mag_int)){
  if (df_temp_E$mag_int[i] > 7 & df_temp_E$mag_int[i]<= 8){
    df_temp_E$mag_int[i] <- df_temp_E$mag_int[i]*2
  }
  if (df_temp_E$mag_int[i] > 6 & df_temp_E$mag_int[i]<= 7){
    df_temp_E$mag_int[i] <- df_temp_E$mag_int[i]*1.8
  }
  if (df_temp_E$mag_int[i] > 5 & df_temp_E$mag_int[i]<= 6){
    df_temp_E$mag_int[i] <- df_temp_E$mag_int[i]*1.6
  }
  if (df_temp_E$mag_int[i] > 4 & df_temp_E$mag_int[i]<= 5){
    df_temp_E$mag_int[i] <- df_temp_E$mag_int[i]*1.4
  }
  if (df_temp_E$mag_int[i] > 3 & df_temp_E$mag_int[i]<= 4){
    df_temp_E$mag_int[i] <- df_temp_E$mag_int[i]*1.2
  }
  if (df_temp_E$mag_int[i] <= 2){
    df_temp_E$mag_int[i] <- df_temp_E$mag_int[i]*0.8
  }
}
# head(df_temp_E)


df_risk <- data.frame(Latitude = df_temp_E$latitude,
                      Longitude = df_temp_E$longitude,
                      Risk_Index = df_temp_E$mag_int)
df_risk[(length(df_temp_E$mag_int)+1):(length(df_temp_E$mag_int)+length(df_temp_V$Explosivness_Index)), ] <- df_temp_V

head(df_risk)

################################################################################
########################### RISK MAP ###########################################
################################################################################

ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region), 
    fill = "lightgray", 
    colour = "black") + 
  geom_point(data = df_risk[which(df_risk$Risk_Index>4),], 
             aes(x = Longitude, y = Latitude, colour = as.integer(Risk_Index)), size = 3
             ) +
  scale_colour_gradient(name = "Risk Index",
                        low = "#fbf8bc",
                      high = "#FF0000") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  ) +
  ggtitle("Distribution of perceivable Earthquakes and Eruptions.")
  
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(x = long, y = lat, map_id = region),
    fill = "lightgray", 
    colour = "black"
  )  +
  geom_bin2d(data = df_risk[which(df_risk$Risk_Index>4),],
             aes(x = Longitude, y = Latitude)) +
  scale_fill_gradient(name = "Frequency",low = "#fbf8bc",
                      high = "#FF0000") +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
    ) +
  ggtitle("Frequency of perceivable Earthquakes and Eruptions.")

