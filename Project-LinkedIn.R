# Loading, analyzing and mapping data (name, industry, location) about Vivan and my LinkedIn connections

library(RCurl)
library(RJSONIO)
library(ggplot2)
library(ggmap)
library(XML)
library(maps)
library(mapproj)
library(plyr)
library(reshape2)

# accessing LinkedIn data from a local file previously scraped in Python

V_raw <-fromJSON('C:\\Users\\David\\Downloads\\linkedin_connections_vivian.json')
D_raw <-fromJSON('C:\\Users\\David\\Downloads\\linkedin_connections_david.json')

#set predefined length of each vector to save time of expanding vector

V_n <- 836
V_Name <- rep("", V_n)
V_Industry <- rep("", V_n)
V_Location <- rep("", V_n)
V_Coordinates <- rep("", V_n)

D_n <- 195
D_Name <- rep("", D_n)
D_Industry <- rep("", D_n)
D_Location <- rep("", D_n)
D_Coordinates <- rep("", D_n)

#fill the V empty vectors with Vivian's data

for (i in 1:V_n){
  
  #fix for the issue of people on private setting
  if (V_raw$values[[i]][[1]] != "private"){
    
    #fix for the issue of people who didn't enter an industry
    if (is.null(V_raw$values[[i]]$industry) == FALSE){
      
      V_Name[i] <- V_raw$values[[i]]$lastName
      V_Industry[i] <- V_raw$values[[i]]$industry
      V_Location[i] <- V_raw$values[[i]]$location$name
      
      #To remove the word "Greater" from the beginning of Location
      if(substr(V_Location[i], start=1, stop=7) == "Greater")
        V_Location[i] <- unlist(strsplit(sub(" ", ";", V_Location[i]), ";"))[2]
    }
  }
}

#Convert the city description into longitude and latitude coordinates

V_Coordinates <- geocode(V_Location)

#Create a dataframe of all data columns, where D_Coordinates becomes
#the columns "lon" and "lat"

V_data <- data.frame(V_Name, V_Industry, V_Location, V_Coordinates)

#fill the D empty vectors with David's data

for (i in 1:D_n){
  
  #fix for the issue of people on private setting
  if (D_raw$values[[i]][[1]] != "private"){
  
    #fix for the issue of people who didn't enter an industry
    if (is.null(D_raw$values[[i]]$industry) == FALSE){
    
      D_Name[i] <- D_raw$values[[i]]$lastName
      D_Industry[i] <- D_raw$values[[i]]$industry
      D_Location[i] <- D_raw$values[[i]]$location$name
      
      #To remove the word "Greater" from the beginning of Location
      if(substr(D_Location[i], start=1, stop=7) == "Greater")
        D_Location[i] <- unlist(strsplit(sub(" ", ";", D_Location[i]), ";"))[2]
    }
  }
}

#Convert the city description into longitude and latitude coordinates

D_Coordinates <- geocode(D_Location)

#Create a dataframe of all data columns, where D_Coordinates becomes
#the columns "lon" and "lat"

D_data <- data.frame(D_Name, D_Industry, D_Location, D_Coordinates)

####################################################################################
################################     MAPPING     ###################################
####################################################################################


########################## GOOGLE MAP: ONLY CONTINENT ##############################

#Read the map data from Google by the ggmap package,
#and mark D_data and V_data on the map.

# zoom=3 (continent) is the most zoomed out option offered by this map.
# maptype = c("terrain", "satellite", "roadmap", "hybrid")
map <- ggmap(get_googlemap(center = 'usa', zoom=3,maptype='terrain'),extent='device') +
  geom_point(data=D_data,aes(x=lon,y=lat),colour = 'red',alpha=1.0)+
  geom_point(data=V_data,aes(x=lon,y=lat),colour = 'blue',alpha=1.0)+
  theme(legend.position = "none")
print(map)

########################## STREETMAP: WHITE WORLD ##############################

library(OpenStreetMap)
map <- openmap(c(70,-179),c(-70,179), zoom=1)
map <- openproj(map)
autoplot(map) +
  geom_point(data=D_data,aes(x=lon,y=lat),colour = 'red',alpha=0.7)+
  geom_point(data=V_data,aes(x=lon,y=lat),colour = 'blue',alpha=0.7)+
  theme(legend.position = "none")

########################## STREETMAP: DARK WORLD ##############################

library(OpenStreetMap)
map <- openmap(c(70,-179),c(-70,179), zoom=1, type="mapquest-aerial")
map <- openproj(map)
autoplot(map) +
  geom_point(data=D_data,aes(x=lon,y=lat),colour = 'white',alpha=1.0)+
  geom_point(data=V_data,aes(x=lon,y=lat), colour = 'yellow',alpha=1.0)

############################## CREATE QUANTITY DATAFRAME ##################################

# create a dataframe with 2 columns, unique location and quantity in that location,
# then add a third column for coordinates

D_data$D_Quantity <- rep(1, 195)
D_dataNew <- ddply(D_data, .(D_Location), summarise, D_Quantity = sum(D_Quantity))
D_dataNew$Coords <- geocode(as.character(D_dataNew$D_Location))
D_dataNew

V_data$V_Quantity <- rep(1, 836)
V_dataNew <- ddply(V_data, .(V_Location), summarise, V_Quantity = sum(V_Quantity))
V_dataNew$Coords <- geocode(as.character(V_dataNew$V_Location))
V_dataNew

################# CHANGE DOT SIZE ON FREQUENCY OF LOCATION - DAVID ONLY #################

library(OpenStreetMap)
map <- openmap(c(70,-179),c(-70,179), zoom=1, type="mapquest-aerial")
map <- openproj(map)
autoplot(map) +
geom_point(data=D_dataNew,
           aes(x=as.numeric(D_dataNew$Coords$lon),
               y=as.numeric(D_dataNew$Coords$lat),
               size = D_Quantity),
           colour = 'white') +
scale_size_continuous(range = c(2, 10))

############## CHANGE DOT SIZE ON FREQUENCY OF LOCATION - DAVID AND VIVIAN ##############

library(OpenStreetMap)
map <- openmap(c(70,-179),c(-70,179), zoom=1, type="mapquest-aerial")
map <- openproj(map)
autoplot(map) +
  geom_point(data=V_dataNew,
             aes(x=as.numeric(V_dataNew$Coords$lon),
                 y=as.numeric(V_dataNew$Coords$lat),
                 size = V_Quantity),
             colour = 'yellow',
             alpha = 1.0) +
  scale_size_continuous(range = c(2, 10)) +
  geom_point(data=D_dataNew,
             aes(x=as.numeric(D_dataNew$Coords$lon),
                 y=as.numeric(D_dataNew$Coords$lat),
                 size = D_Quantity),
             colour = 'white',
             alpha = 1.0) +
  opts(title = expression('LinkedIn Connections around the WORLD'))

############################ EXAMPLES OF SIZE CHANGE ###################################

p <- ggplot(data=mpg, mapping=aes(x=cty,y=hwy)) +
  geom_point(aes(colour=class,size=displ),
             alpha=0.5,position = "jitter") +
  geom_smooth() +
  scale_size_continuous(range = c(4, 10)) +
  facet_wrap(~ year,ncol=1) +
  opts(title='Vehicle model and fuel consumption') +
  labs(size='Displacement', colour = 'Model')
print(p)


p <- ggplot(data=mpg,aes(x=cty,y=hwy)) +
  geom_point(aes(color=year,size=displ),alpha=0.5,position = "jitter") +
  geom_smooth(method='lm') +
  scale_size_continuous(range = c(4, 10))
print(p)


#further steps:

#add legend
#transparancy and jitter the points
#size of the circle (mpg example)
#igraph:social network analysis(plot function)
#rbloggers:maps, social network analysis
#color coade states on a us map showing who has more connections in each
#delete records with missing values



