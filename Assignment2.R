#Putting  $550 billion in infrastructure is at the heart of US President Trump's 
#governance framework as the news says, and he has claimed to rebuild a reliable 
#and efficient US infrastructure, "We will rebuild our infrastructure and Built
#roads, bridges, airports, transit systems and ports to make the world jealous, 
#improve the quality of life for all Americans, "he said so. The idea is very 
#good, the reality is very cruel. December 1,2016 the United States, "Washington
#Post" gave Trump poured cold water. According to the data from various government
#sources,  it’s somewhat difficult  to achieve Trump's plan.


install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("choroplethrMaps")
install.packages("ggplot2")
install.packages("ggmap",type="source")
install.packages("mapproj")

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(choroplethrMaps)
library(ggplot2)
library(ggmap)
library(mapproj)

#1.prepare the basemap with the help of google.map
##get map of USA
mapUSA <- get_googlemap(center  = c(-95.71289,38),zoom=4,
                        maptype="terrain",language = "en-En",size = c(640,360))
ggmap(mapUSA)
##get map of delaware
mapDE <- get_googlemap(center  = c(-75.40,39.15),zoom=9,
                       maptype="terrain",language = "en-En",size = c(360,700))
ggmap(mapDE)

#2.download the data and data processing and wrangling
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()
dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008", "COUNTY_CODE_003", 
         "LAT_016", "LONG_017", "YEAR_BUILT_027",   "FRACTURE_092A", 
         "HISTORY_037", "STRUCTURE_LEN_MT_049","RAILINGS_036A",
         "TRANSITIONS_036B","APPR_RAIL_036C","APPR_RAIL_END_036D")
M = select(x16,one_of(keep))
M = as.tbl(M)
min2dec = function(x){
  len = nchar(x)
  inte = as.numeric(substr(x,1,len-6))
  decim = as.numeric(substr(x,len-5,len))/6e+05
  return(inte+decim)
}
M = mutate(M,lat = min2dec(LAT_016), lon = -min2dec(LONG_017))
ggplot(data = M) + geom_point(mapping = aes(y = lat, x = lon))
M = M %>% filter(lon>-130,lat<50,lat>25)
ggplot(data = M) + geom_point(mapping = aes(y = lat, x = lon))
M = M %>% filter(lon< -60)

pdf("USbridge1.pdf",family="GB1")
ggmap(mapUSA) + geom_point(data = M,size=0.001) + coord_fixed()
dev.off()


#3.visualization

## A study by the American Road & Transportation Builders Association in 2016 found that
## nearly 10% of the 600,000 bridges in the United States were structurally deficient. 
## Show the structurally deficient condition of bridge all over the USA.

##I create a new variable”safety”,which is the sum of  "RAILINGS_036A”,
##”TRANSITIONS_036B","APPR_RAIL_036C","APPR_RAIL_END_036D", to measure the degree 
##of structural defects. The rate is 1 to 5, which means least safe to safest.

M1 <- M %>% 
  filter(RAILINGS_036A!="N",
                   TRANSITIONS_036B!="N",
                   APPR_RAIL_036C!="N",
                   APPR_RAIL_END_036D!="N") %>%
  mutate(safety = as.numeric(RAILINGS_036A)+
           as.numeric(TRANSITIONS_036B)+
           as.numeric(APPR_RAIL_036C)+
           as.numeric(APPR_RAIL_END_036D)+1) %>%
  filter(!is.na(safety)) %>% 
  mutate(latest=(YEAR_BUILT_027>1966))
M1$latest[M1$latest==1] = "after1966"
M1$latest[M1$latest==F] = "before1966"
#ggmap(mapUSA,
#      base_layer=ggplot(aes(x=lon,y=lat,color=M1$safety),data=M1) + 
#        geom_point(size=0.001,show.legend = T))
pdf("USbridge_safety.pdf",family="GB1")
ggplot(data=M1,aes(x=lon,y=lat,color=safety)) +
  geom_point(size=0.001,show.legend = T,alpha=0.5) + coord_fixed()
dev.off()
pdf("USbridge_safety_latest50.pdf",family="GB1")
ggplot(data=M1 %>% filter(YEAR_BUILT_027>1966),aes(x=lon,y=lat,color=safety)) +
  geom_point(size=0.001,show.legend = T,alpha=0.5) + coord_fixed()
dev.off()


## show the distribution of old bridges (defined by "built during 1920-1930" ) 
## all over the USA ,easily to find that old bridges has comparitive lower safety
## ratings.
pdf("USbridge_1920_1930.pdf",family="GB1")
ggplot(data=M1 %>% filter(YEAR_BUILT_027>1920,YEAR_BUILT_027<1930),
       aes(x=lon,y=lat,color=safety)) +
  geom_point(show.legend = T,alpha=0.3) + coord_fixed()
dev.off()


##According to the previous study,Delaware bridges are much more modern, but 
##75% of "structural defects" of the bridge is built in the past 50 years.
## show the structurally deficient condition of  modern bridges(defined by 
## "built in the past 50 years") all over the USA (DE)
de = M1 %>% filter(STATE_CODE_001==10)
pdf("deBridge.pdf",family="GB1")
ggmap(mapDE,base_layer=ggplot(aes(x=lon,y=lat,color=de$safety),data=de)) +
  geom_point(show.legend = T) +
  facet_grid(.~latest) + coord_fixed()
dev.off()
