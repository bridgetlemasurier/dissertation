#################################################################
############ Preprocessing-- defining presence & absence

setwd("sdm_course/sections/data/bioclim")
library(raster)

datafiles = Sys.glob("*.tif") #Or whatever identifies your files
stck = stack() #empty stack for raster
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

stck #raster predictors as a stack

plot(stck,1)

### presence data

horn=read.csv("hornbill_my1.csv")
head(horn)
horn1= horn[,-1]#first column not needed


points(horn1, col='blue') 

# extract data at each point of presenence ----

prs1= extract(stck, horn1) # extract stck info at each point in horn1


# create pseudo-absences ----
set.seed(1) # so we select the same sample

backgr = randomPoints(stck, 500) #500 random points
absvals = extract(stck, backgr) #choose absence values from the background


# store presences and absences----
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals))) 
sdmdata = data.frame(cbind(pb, rbind(prs1, absvals)))

head(sdmdata)

sdmdata=na.omit(sdmdata)
summary(sdmdata)

tail(sdmdata)

# select area based on ecology ----
e = drawExtent() # dynamically select absence

abs = crop(stck, e) # crop raster stack on envs to box of known absence
plot(abs)

backgr = randomPoints(abs, 135) # 80:20 ratio - can do this based on data
absvals2 = extract(abs, backgr) # 400 pseudo-absences

pb = c(rep(1,nrow(prs1)), rep(0,nrow(absvals2)))

sdmdata = data.frame(cbind(pb, rbind(prs1, absvals2)))

head(sdmdata)

write.csv(sdmdata, "Pres_abs.csv")
