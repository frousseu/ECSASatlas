
library(scales)
library(sp)
library(rgeos)
library(dplyr)
library(ECSASconnect)
library(GeoAviR)
library(rgdal)
library(readxl)
library(TeachingDemos)
library(hexbin)
library(readxl)
library(plyr)
library(RODBC)
library(magrittr)
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(leaflet)
library(xlsx)
library(classInt)
library(FRutils)
library(tidyr)
library(RCurl)
library(jpeg)
library(png)
#load("C:/Users/User/Documents/SCF2016_FR/yo9.RData")


###########################################
### INIT
###########################################


pathECSAS<-"C:/Users/User/Documents/SCF2016_FR/ECSASdata"#"Y:/Inventaires/Pélagiques/ECSAS"
fileECSAS<-"Master ECSAS v 3.51.mdb"


### get groups from github
groups<-getURL("https://raw.githubusercontent.com/frousseu/NEECbirds/master/bird_groups.csv") # Ce fichier est sur mon github
groups<-read.csv(text=groups,header=TRUE,stringsAsFactors=FALSE)

### get data for french names from EC's official list on my github
spname<-getURL("https://raw.githubusercontent.com/frousseu/ECSASatlas/master/EC_AVIAN_CORE_20161216.csv",.encoding="LATIN1") # Ce fichier est sur mon github
spname<-read.csv(text=spname,header=TRUE,stringsAsFactors=FALSE)


### get ECSAS database
ecsas<-ECSAS.extract(lat=c(39.33489,74.65058),long=c(-90.50775,-38.75887),sub.program=c("Atlantic","Quebec"),ecsas.drive=pathECSAS,ecsas.file=fileECSAS)
ecsas$English<-as.character(ecsas$English)

### path pour MCDS.exe
pathMCDS<-"C:/Users/User/Documents/SCF2016_FR" #path pour MCDS.exe


### différentes projections utilisées
ll<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
prj<-"+proj=utm +zone=22 +datum=NAD83 +ellps=GRS80"
laea<-"+proj=laea +lat_0=50 +lon_0=-65"


### GET LAND SHAPEFILES
## comes form natural earth
pathshp<-"C:/Users/User/Documents/SCF2016_FR/shapefiles"
eu<-readOGR(dsn=pathshp,layer="ne_10m_admin_0_countries",encoding="UTF-8")
eu<-eu[eu$GEOUNIT%in%c("Greenland","Iceland","United Kingdom","Ireland","France","Spain","Portugal"),]
eu<-spTransform(eu,CRS(laea))
gr<-eu[eu$ADMIN=="Greenland",]
na<-readOGR(dsn=pathshp,layer="ne_10m_admin_1_states_provinces",encoding="UTF-8")
na<-na[na$admin%in%c("United States of America","Canada"),]
na<-gIntersection(na,bbox2pol(na,ex=-1),byid=TRUE)
na<-spTransform(na,CRS(laea))
boxcut<-c(-1848241,3712932,-1413652,3035287) ####!!!!!! On coupe avec cette bbox (déterminée en produisant les pngs avec l'objet boxcut) pour éviter les problèmes de transparence liés au dépassement des polygones des cadres
na<-gIntersection(na,bbox2pol(boxcut,ex=-1,proj4string=laea),byid=TRUE)
eu<-gIntersection(eu,bbox2pol(boxcut,ex=-1,proj4string=laea),byid=TRUE)
gr<-gIntersection(gr,bbox2pol(boxcut,ex=-1,proj4string=laea),byid=TRUE)
gl<-readOGR(dsn=pathshp,layer="ne_10m_lakes",encoding="UTF-8")
gl<-gl[gl$name%in%c("Lake Ontario","Lake Huron","Lake Erie","Lake Michigan","Lake Superior"),]
gl<-spTransform(gl,CRS(laea))
b0<-spTransform(readOGR(dsn=pathshp,layer="b0",encoding="UTF-8"),CRS(laea))
b200<-spTransform(readOGR(dsn=pathshp,layer="b200",encoding="UTF-8"),CRS(laea))
b1000<-spTransform(readOGR(dsn=pathshp,layer="b1000",encoding="UTF-8"),CRS(laea))
b2000<-spTransform(readOGR(dsn=pathshp,layer="b2000",encoding="UTF-8"),CRS(laea))
b3000<-spTransform(readOGR(dsn=pathshp,layer="b3000",encoding="UTF-8"),CRS(laea))
b4000<-spTransform(readOGR(dsn=pathshp,layer="b4000",encoding="UTF-8"),CRS(laea))
b5000<-spTransform(readOGR(dsn=pathshp,layer="b5000",encoding="UTF-8"),CRS(laea))
b6000<-spTransform(readOGR(dsn=pathshp,layer="b6000",encoding="UTF-8"),CRS(laea))



### extract QC data that is not in ECSAS yet
addQC<-SOMEC2ECSAS(input="C:/Users/User/Documents/SCF2016_FR/ECSASdata/SOMEC.accdb",output="C:/Users/User/Documents/SCF2016_FR/ECSASdata/ECSASexport.csv",date="2014-04-01",step="5 min",spNA=FALSE)
names(addQC)<-gsub("Orig","",names(addQC))
# For now, elements without WatchID are scrapped, it may be due to a bug in the SOMEC2ECSAS function or missing data in the original files. Check for that and add a comment in the TODO list of ECSASconnect
addQC<-addQC[!is.na(addQC$WatchID),]


### MAKE SURE SOMEC database is ok when I LEAVE!!!!!!!!!!!!
### Replace french names and wrong names in Quebec data
### There are about 15 observations without a count number table(addQC$Alpha[is.na(addQC$Count)],useNA="always")
db<-odbcConnectAccess2007("C:/Users/User/Documents/SCF2016_FR/ECSASdata/SOMEC.accdb") ### nedd a last update to SOMEC and make sure it is correct compared to the old version
qc<-sqlFetch(db,"Code espèces",as.is=TRUE)
odbcClose(db)
bn<-c("foba","fubo","fulm","GOAC","goar","guma","LALQ","LIMICOLESP","OCWL","PATC","PLON","RAZO","rien","SCSP")
gn<-c("FOBA","FUBO","FULM","GOAC","GOAR","GUMA","LALQ","LIMICOLESP","OCWI","PATC","PLON","PEPI","RIEN","SCSP")
m<-match(addQC$Alpha,bn)
addQC$Alpha<-ifelse(is.na(m),addQC$Alpha,gn[m])
m<-match(addQC$Alpha,qc$CodeFR)
addQC$Alpha<-ifelse(!is.na(m),qc$CodeAN[m],addQC$Alpha)
addQC$add<-1 # pour v?rifier les lignes restantes ? la fin
addQC$Date<-ifelse(is.na(addQC$Date),substr(addQC$StartTime,1,10),addQC$Date)
m<-match(addQC$Alpha,ecsas$Alpha)
addQC$English<-ifelse(!is.na(m),ecsas$English[m],"")

###########CEHCK FOR Count NA in data in addQC and final d object
########### ALSO missing French names in Long-tailed Jaegers


### build complete database
#d<-ecsas
d<-join(ecsas,addQC,type="full")
#d$Date<-substr(d$Date,1,10)
d$Alpha<-ifelse(is.na(d$Alpha) | d$Alpha%in%c("RIEN","NOBI"),"",as.character(d$Alpha))
d$English<-ifelse(is.na(d$English),"",as.character(d$English))
d$French<-spname$French_Name[match(d$English,spname$English_Name)]
d$Distance<-ifelse(is.na(d$Distance),"",as.character(d$Distance))
dl<-c("A","B","C","D","c")
dn<-c("25","75","150","250","150")
m<-match(d$Distance,dl)
d$Distance<-ifelse(is.na(m),d$Distance,dn[m])
d<-d[which(d$LongStart>(-150) & d$LongStart<(-18)),] ### j'enlève la croisière partant vers l'uerope car créer des distortions dans les projections
d<-d[which(d$LatStart>(0) & d$LatStart<(90)),]
#d<-d[which(d$InTransect%in%c(-1)),] les transects vides ont des NA
d<-distance.filter(d,distance.labels=c(25,75,150,250))
d<-d[order(d$CruiseID,d$WatchID,d$Date,substr(d$StartTime,12,19)),]
d$Month<-substr(d$Date,6,7)
month_comb<-c("12010203","04050607","08091011")
d$MonthC<-month_comb[sapply(d$Month,function(i){
  g<-grep(i,month_comb)
  if(length(g)>1){
    if(i=="01"){g<-g[1]}
    if(i=="10"){g<-g[2]}
  }
  g
})]


### BUILD GRID
dproj<-SpatialPointsDataFrame(SpatialPoints(matrix(c(d$LongStart,d$LatStart),ncol=2),CRS(ll)),data=d)
dproj<-spTransform(dproj,CRS(laea))
grid<-hexgrid(dproj,width=100000,convex=FALSE,seed=111)
grid2<-hexgrid(bbox2pol(dproj),width=100000,convex=FALSE,seed=111) # a second "complete" grid to find out where are unvisited cells
grid$id<-paste0("g",grid$id)


### GET DATA THAT IS IN CELLS ---------------------------
dshp<-SpatialPointsDataFrame(SpatialPoints(matrix(c(d$LongStart,d$LatStart),ncol=2),CRS(ll)),data=d)
o<-over(spTransform(dshp,CRS(proj4string(grid))),grid)
d$STR_LABEL<-o$id
d$cell<-o$id
d<-d[!is.na(d$STR_LABEL),]


##############################################
### GROUPINGS ddd<-d
##############################################

# there is a group for detection and a group for the atlas product

d$English<-as.character(d$English)
d$English[grep("Genus: Gulls",d$English)]<-"Genus: Gulls"
m<-match(d$English,groups$sp)

# Check what does not have a name in the group file
sort(table(d$English[m]))

# If no species are in English because of an empty transect, we don't want ;a group name
d$group_detection<-ifelse(!d$English%in%c("",NA),groups$group_detection[m],"")
d$group_atlas<-ifelse(!d$English%in%c("",NA),groups$group_atlas[m],"")

# Check what does not have a group name to make sure it is not an important species 
empty<-is.na(d$group_detection) | d$group_detection==""
sort(table(d$English[empty]))
empty<-is.na(d$group_atlas) | d$group_detection==""
sort(table(d$English[empty]))

# Turn what does not have a group to empty values in species, distance, groups and count

k1<-!d$Alpha%in%c("",NA) & d$group_detection%in%c("",NA)
k2<-!d$Alpha%in%c("",NA) & d$group_atlas%in%c("",NA)

d$Alpha<-ifelse(k1 & k2,"",d$Alpha)
d$English<-ifelse(k1 & k2,"",d$English)
d$Count<-ifelse(k1 & k2,"",d$Count)
d$Distance<-ifelse(k1 & k2,"",d$Distance)
d$group_detection<-ifelse(k1 & k2,"",d$group_detection)
d$group_atlas<-ifelse(k1 & k2,"",d$group_atlas)


###########################################
### GLOBAL MODELS
###########################################

### run model without empty transect to only get detection probability and the maximum number of observations compared to 32767

group_detection_list<-unique(d$group_detection) 
group_detection_list<-group_detection_list[!group_detection_list%in%c("",NA)]

### build new Watch IDs with days/cruise
d$SMP_LABEL<-paste(d$CruiseID,d$Date,d$cell,sep="_")
temp<-ddply(d,.(SMP_LABEL),function(i){
	sum(i$WatchLenKm[!duplicated(i$WatchID)])
})
d$SMP_EFFORT<-round(temp$V1[match(d$SMP_LABEL,temp$SMP_LABEL)],2)
d<-d[order(d$Date,d$CruiseID,d$cell),]

### number of empty watches
ans<-ddply(d,.(SMP_LABEL),function(x){
	if(all(x$Alpha=="")){
		"empty" 
	}else{
		if(any(x$Alpha=="") && any(x$Alpha!="")){
			"mix"
		}else{
			"obs"
		}
	}
})
table(ans$V1)
d$ans<-ans$V1[match(d$SMP_LABEL,ans[,"SMP_LABEL"])]
d$ans2<-!(d$ans=="mix" & d$Alpha=="")
d<-d[!(d$ans=="mix" & d$Alpha==""),] #eliminate empty watches lines from transects
d<-d[!(d$ans=="empty" & duplicated(d$SMP_LABEL)),]  #keep a single line for empty transects
d<-d[!is.na(d$Distance),]


### Global Detection Model
ds<-d  # ? 32768 SMP_LABEL, MCDS plante à 32768 label
ds$SMP_LABEL<-as.numeric(factor(ds$SMP_LABEL))
ds$STR_AREA<-gArea(grid[1,])/1000/1000
length(unique(ds$SMP_LABEL))

mg<-distance.wrap(ds,
    SMP_EFFORT="SMP_EFFORT",
    DISTANCE="Distance",
    SIZE="Count",
    units=list(Type="Line",
      Distance="Perp",
      Length_units="Kilometers",
      Distance_units="Meters",
      Area_units="Square kilometers"),
    breaks=c(0,50,100,200,300), 
    estimator=list(c("HN","CO")),
    lsub=list(group_detection=group_detection_list), 
    empty=NULL,
    split=TRUE,
    STR_AREA="STR_AREA",
    SMP_LABEL="SMP_LABEL",
    STR_LABEL="STR_LABEL",
    path="C:/Users/User/Documents/SCF2016_FR/ECSASatlas/distance.wrap.output",
    pathMCDS=pathMCDS
)



### number of watches and observations for each group
nbobs1<-ddply(d,.(group_atlas,group_detection,Alpha,MonthC),function(x){
  #nb_sample<-length(unique(d$SMP_LABEL[d$MonthC==x$MonthC[1]]))
  nb_obs<-sum(x$Alpha!="")
  nb_obs
})
names(nbobs1)[1:5]<-c("group_atlas","group_detection","Alpha","MonthC","nb_obs")
nbobs1<-nbobs1[nbobs1$Alpha!="",]

### number of watches and observations for each group
nbobs2<-ddply(d,.(group_atlas,group_detection,MonthC),function(x){
	#nb_sample<-length(unique(d$SMP_LABEL[d$MonthC==x$MonthC[1]]))
	nb_obs<-sum(x$Alpha!="")
	nb_obs
})
names(nbobs2)[1:4]<-c("group_atlas","group_detection","MonthC","nb_obs")
nbobs2$Alpha<-""
nbobs2<-nbobs2[,names(nbobs1)]

nbobs<-rbind(nbobs2,nbobs1)
nbobs<-spread(nbobs,MonthC,nb_obs,fill=0)

### number of cells for each MonthC
ddply(d,.(MonthC),function(x){
  length(unique(x$cell))
})

### BUILD MULTIPLIER LIST WITH PROBABILITY OF DETECTION FOR SPECIES AND GROUPS
temp<-sapply(group_detection_list,function(i){
  x<-mg[[i]]$parameter_estimates$Global
  x$Estimates[x$Parameters=="p"]
})

g<-unique(groups[,c("group_detection","group_atlas")])
g<-g[!(g[,1]=="" | g[,2]==""),]

temp<-temp[g$group_detection]
cond<-names(temp)!="Dovekie"
names(temp)[cond]<-g$group_atlas[cond]

mult_list<-temp
mult_list<-mult_list[match(d$group_atlas[match(unique(d$Alpha),d$Alpha)],names(mult_list))]
names(mult_list)<-unique(d$Alpha)
mult_list<-mult_list[!is.na(mult_list)]
mult_list<-c(mult_list,temp) ### added values for groups
mult_list<-unlist(mult_list)

# add good Dovekie value
mult_list[which(names(mult_list)=="DOVE")]<-unname(mult_list[which(names(mult_list)=="Dovekie")]) 
# add Murres
mult_list<-c(mult_list,Murres=unname(mult_list[which(names(mult_list)=="Alcids")]))



### BUILD DATA FOR EACH SPECIES
dl1<-dlply(d[d$Alpha!="",],.(Alpha,MonthC),function(x){ # data list
  y<-d[d$MonthC==x$MonthC[1],]
  w1<-which(y$Alpha!=x$Alpha[1])
  y$Empty<-0
  y$Empty[w1]<-1
  y$Alpha[w1]<-""
  y$Count[w1]<-""
  y<-y[order(y$SMP_LABEL,y$Empty),]
  y[which(y$Empty==0 | !duplicated(y$SMP_LABEL)),]
})

### BUILD DATA FOR EACH GROUP
d2<-d
d2$Alpha<-ifelse(d2$Alpha=="",d2$Alpha,d2$group_atlas)
dl2<-dlply(d2[d2$Alpha!="",],.(Alpha,MonthC),function(x){ # data list
	y<-d2[d2$MonthC==x$MonthC[1],]
	w1<-which(y$Alpha!=x$Alpha[1])
	y$Empty<-0
	y$Empty[w1]<-1
	y$Alpha[w1]<-""
	y$Count[w1]<-""
	y<-y[order(y$SMP_LABEL,y$Empty),]
	y[which(y$Empty==0 | !duplicated(y$SMP_LABEL)),]
})

### BUILD DATA FOR Murres
d3<-d
d3$Alpha<-ifelse(d3$Alpha%in%c("UNMU","TBMU","COMU"),"Murres","")
dl3<-dlply(d3[d3$Alpha!="",],.(Alpha,MonthC),function(x){ # data list
	y<-d3[d3$MonthC==x$MonthC[1],]
	w1<-which(y$Alpha!=x$Alpha[1])
	y$Empty<-0
	y$Empty[w1]<-1
	y$Alpha[w1]<-""
	y$Count[w1]<-""
	y<-y[order(y$SMP_LABEL,y$Empty),]
	y[which(y$Empty==0 | !duplicated(y$SMP_LABEL)),]
})

### APPEND ALL LISTS AND SUBSET ACCORDING TO A NUMBER OF OBSERVATIONS
dl<-c(dl1,dl2,dl3)
keep<-sapply(dl,function(x){sum(x$Alpha!="")})
dl<-dl[keep>10] #on enlève ce qui n'a pas assez d'observations
w<-which(substr(names(dl),1,2)=="UN" | substr(names(dl),1,4)=="ALCI" | substr(names(dl),1,7)=="Gannets") 
dl<-dl[-w] #on enlève les espèces inconnues, les ALCI et les Gannets
ml<-vector(mode="list",length=length(dl))
names(ml)<-names(dl)



### ADD A TOTAL


###########################################
### GROUP MODELS
###########################################

#ALSP.08091011 ne run pas pour une raison obscure

for(i in seq_along(dl)){
  
   x<-dl[[i]]
   #x<-x[x$SMP_LABEL%in%sample(unique(x$SMP_LABEL),100),]
   mult<-mult_list[match(sapply(strsplit(names(dl)[i],"\\."),function(x){x[1]}),names(mult_list))]
   x$SMP_LABEL<-as.numeric(as.factor(x$SMP_LABEL))
   x$STR_AREA<-gArea(grid[1,])/1000/1000
   x$Distance<-ifelse(x$Count=="","",x$Distance)
   
   m<-distance.wrap(x, 
     SMP_EFFORT="SMP_EFFORT",
     DISTANCE="Distance",
     SIZE="Count",
     units=list(Type="Line",
       Distance="Perp",
       Length_units="Kilometers",
       Distance_units="Meters",
       Area_units="Square kilometers"),
     breaks=c(0,50,100,200,300), 
     estimator=list(c("UN","CO")),
     #lsub=list(Period=unique(x$Period)), 
     empty=NULL,
     #split=TRUE,
     STR_AREA="STR_AREA",
     SMP_LABEL="SMP_LABEL",
     STR_LABEL="cell",
     stratum="STR_LABEL",
     path="C:/Users/User/Documents/SCF2016_FR/ECSASatlas/distance.wrap.output",
     pathMCDS=pathMCDS,
     multiplier=2/mult
   )
   print(names(ml)[i])
   ml[[i]]<-m
}

#ml<-unlist(ml,recursive=FALSE)
#names(ml)<-gsub("\\.","_",names(ml))

#global.summary.distanceList(model=ml,species=NULL,file="temp",directory="C:/Users/rousseuf/Documents")




###########################################
### PRODUCE ATLAS FIGURES
###########################################

source("C:/Users/user/Documents/temp_maps.R",encoding="UTF-8")

###########################################
### PRODUCE ATLAS FIGURES
###########################################


#grid2<-spTransform(grid2,CRS(laea))

#grid<-spTransform(grid,CRS(laea))
#reg_atl<-readOGR("C:/Users/User/Documents/SCF2016_FR/shapefiles",layer="ATLBioregions",verbose=FALSE)
#reg_atl<-spTransform(reg_atl,CRS(proj4string(grid)))

groupn<-c(Terns="Sternes",Shearwaters="Puffins","Storm-Petrels"="Océanites","Diving Waterfowl"="Canards plongeurs",Phalaropes="Phalaropes",Jaegers="Petits labbes",Skuas="Grands labbes",Alcids="Alcidés",Gulls="Goélands et mouettes",Murres="Guillemot marmette ou Marmette de Brünnich")

hex<-grid[1,]
row.names(hex@data)<-sapply(slot(hex,"polygons"),function(x){slot(x,"ID")})

cols<-rev(c("darkred",colo.scale(seq(0,1,length.out=3),c("red","white"))))
trans<-0.65
mag<-1
tex<-0.6
monthNB<-list(c(12,1:3),4:7,8:11)
lgroup<-names(ml)
#lgroup<-c("WWSC.12010203")
ldens<-vector(mode="list",length(lgroup))
names(ldens)<-lgroup
i<-1

for(i in seq_along(lgroup)){
	
	group<-lgroup[i]
	
	if(class(ml[[group]])=="character"){ # this is for models that do not run
		next
	}
	
	png(paste0("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/maps/",gsub("\\.","_",group),".png"),width=6,height=4.8,units="in",res=600)
	
	dens<-density.map(ml[[group]],by.stratum=TRUE)
	dat<-dl[[group]]
	temp<-ddply(dat,.(cell),function(k){length(unique(k$SMP_LABEL))})
	names(temp)<-c("Region","nbsamp")
	dens<-join(dens,temp)
	ldens[[i]]<-cbind(Group=unlist(strsplit(group,"\\."))[1],Month=unlist(strsplit(group,"\\."))[2],dens,stringsAsFactors=FALSE)
	grid$val<-dens$Estimates[match(grid$id,dens$Region)]
	grid$u<-dens$"95% Upper"[match(grid$id,dens$Region)]
	grid$l<-dens$"95% Lower"[match(grid$id,dens$Region)]
	grid$u<-ifelse(grid$u>(2*max(grid$val,na.rm=TRUE)),2*max(grid$val,na.rm=TRUE),grid$u)
	grid$cv<-dens$"% of var."[match(grid$id,dens$Region)]
	grid$diff<-grid$u-grid$l
	count<-ddply(dl[[group]],.(cell),function(i){round(sum(as.numeric(i$Count),na.rm=TRUE)/sum(i$WatchLenKm[!duplicated(i$WatchID)],na.rm=TRUE),1)})
	count<-ddply(dl[[group]],.(cell),function(i){sum(as.numeric(i$Count),na.rm=TRUE)})
	count<-ddply(dl[[group]],.(cell),function(i){paste(sum(as.numeric(i$Count),na.rm=TRUE),round(sum(i$WatchLenKm[!duplicated(i$WatchID)],na.rm=TRUE),0),collapse="\n")})
	grid$count<-count$V1[match(grid$id,count$cell)]
	#grid$cv<-((dens$"95% Upper"[match(grid$id,dens$Region)]-dens$"95% Lower"[match(grid$id,dens$Region)])/grid$val)*100
	#grid$cv<-ifelse(grid$cv>300,300,grid$cv)
	
	grid$nbsamp<-dens$"nbsamp"[match(grid$id,dens$Region)]
	
	#r<-range(c(grid$val,grid$u,grid$l),na.rm=TRUE)
	r<-range(c(grid$val),na.rm=TRUE)
	
	val<-grid$val
	val<-val[!is.na(val)]
	if(length(unique(val))<length(cols)){  # if the number of values is below the length of cols, classIntervals returns non-sense
		val<-sort(c(val,seq(min(val),max(val),length.out=length(cols))))
	}
	br<-suppressWarnings(classIntervals(unique(val), n=length(cols), style = "kmeans", rtimes = 1)$brks)
	br<-ifelse(br<0,0,br)
	
	#br<-NULL
	
	grid$col<-colo.scale(c(r,grid$val),cols=cols,breaks=br)[-(1:2)]
	grid$col<-alpha(ifelse(is.na(grid$cv),NA,grid$col),trans)
	grid$colu<-colo.scale(c(r,grid$u),cols=cols,breaks=br)[-(1:2)]
	grid$colu<-alpha(grid$colu,ifelse(is.na(grid$colu),1,trans))
	grid$coll<-colo.scale(c(r,grid$l),cols=cols,breaks=br)[-(1:2)]
	grid$coll<-alpha(grid$coll,ifelse(is.na(grid$coll),1,trans))
	ncv<-4
	
	val<-grid$cv
	val<-val[!is.na(val)]
	if(length(unique(val))<ncv){  # if the number of values is below ncv, classIntervals returns non-sense and this is a hack to get a range of values
		val<-sort(c(val,seq(2,max(val),length.out=ncv)))
	}
	brcv<-suppressWarnings(classIntervals(unique(val), n=ncv, style = "kmeans", rtimes = 1)$brks)
	brcv<-ifelse(brcv<0,0,brcv) # hack to make sure all is over 0
	cexcv<-seq(0.35,1,length.out=4)
	cutcv<-cut(grid$cv,breaks=brcv)
	grid$cutcv<-cutcv
	grid$cex<-cexcv[as.numeric(cutcv)]*mag
	
	holes<-gBuffer(SpatialPoints(coordinates(grid),proj4string=CRS(proj4string(grid))),width=ifelse(is.na(grid$cex),0,grid$cex*40000),byid=TRUE) # the buffer width value needs to be adjusted manually with the cex of the legend
	gholes<-gIntersection(gDifference(grid,holes),grid,byid=TRUE)
	#plot(gholes,add=TRUE,col="blue")
	
	### PLOT
	par(mar=c(0,0,0,0),mgp=c(0.5,0.1,0))
	k<-!is.na(grid$val)
	plot(grid,bg="#7AAFD1",border="#7AAFD1",xaxs="i",yaxs="i",xlim=c(-1848241,3712932),ylim=c(-1413652,3035287))
	
	boxcut<-par("usr")
	
	#plot(grid)
	#plot(map.osm2,xlim=bbox(grid)[1,],ylim=bbox(grid)[2,],add=TRUE)
	#plot(grid[k,],col="white",bg=alpha("lightblue",0.5),border="grey75",xlim=bbox(grid)[1,],ylim=bbox(grid)[2,],add=TRUE)
	
	l<-c(75,72,69,66,63,60,57,54)-5 #c(70,65,60,55,50,45,42,39)
	plot(b0,col=hcl(240,50,l[1]),border=NA,add=TRUE)
	plot(b200,col=hcl(240,50,l[2]),border=NA,add=TRUE)
	plot(b1000,col=hcl(240,50,l[3]),border=NA,add=TRUE)
	plot(b2000,col=hcl(240,50,l[4]),border=NA,add=TRUE)
	plot(b3000,col=hcl(240,50,l[5]),border=NA,add=TRUE)
	plot(b4000,col=hcl(240,50,l[6]),border=NA,add=TRUE)
	plot(b5000,col=hcl(240,50,l[7]),border=NA,add=TRUE)
	plot(b6000,col=hcl(240,50,l[8]),border=NA,add=TRUE)
	pb<-hcl(240,50,l[1])
	db<-hcl(240,50,l[7])
	
	
	### draw latitudes
	m<-expand.grid(seq(-160,20,by=0.2),seq(25,85,by=5))
	lat<-SpatialPoints(m,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	lat<-spTransform(lat,CRS(proj4string(grid)))
	plot(lat,add=TRUE,col="grey20",pch=16,cex=0.01)
	
	xxlat<-par("usr")[1]
	m<-expand.grid(xxlat,seq(par("usr")[3],par("usr")[4],by=100))
	p<-SpatialPoints(m,proj4string=CRS(proj4string(grid)))
	p2<-spTransform(p,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	r<-range(coordinates(p2)[,2])
	r<-5*round(r/5) 
	selat<-seq(r[1],r[2],by=5)
	yylat<-sapply(selat,function(k){
		coordinates(p)[which.min(abs(coordinates(p2)[,2]-k)),2]	
	})
	
	### draw longitudes
	m<-expand.grid(seq(-160,20,by=10),seq(25,85,by=0.1))
	lon<-SpatialPoints(m,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	lon<-spTransform(lon,CRS(proj4string(grid)))
	plot(lon,add=TRUE,col="grey20",pch=16,cex=0.01)
	
	yylon<-par("usr")[3]+100000
	m<-expand.grid(seq(par("usr")[1],par("usr")[2],by=100),yylon)
	p<-SpatialPoints(m,proj4string=CRS(proj4string(grid)))
	p2<-spTransform(p,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	r<-range(coordinates(p2)[,1])
	r<-10*round(r/10) 
	selon<-seq(r[1],r[2],by=10)
	xxlon<-sapply(selon,function(k){
		coordinates(p)[which.min(abs(coordinates(p2)[,1]-k)),1]	
	})
	
	
	### plot grid and values
	plot(eu,add=TRUE,lwd=0.1,border=NA,col="grey75")
	plot(na,add=TRUE,lwd=0.1,border=NA,col="grey75")
	
	### PLOT GRID WITH HOLES OR NOT
	#plot(grid[k,],col=grid$col[k],border=ifelse(is.na(grid$col[k]),alpha("lightblue",trans),NA),add=TRUE,lwd=0.5)
	plot(gholes[k],col=grid$col[k],border=ifelse(is.na(grid$col[k]),alpha("lightblue",trans),NA),add=TRUE,lwd=0.5)
	
	### PLOT Atalntic region
	#plot(reg_atl,border=alpha("darkgreen",0.25),lwd=2,add=TRUE)
	
	### PLOT unvisited holes in grid
	grid[k,] %>% 
		gUnaryUnion %>% 
		slot("polygons") %>% 
		extract2(1) %>% 
		slot("Polygons") %>% 
		sapply(function(i){!slot(i,"hole")})->
		keep
	
	test<-grid[k,] %>% 
		gUnaryUnion %>% 
		slot("polygons") %>% 
		extract2(1) %>% 
		slot("Polygons") %>%
		magrittr:::extract(keep) %>% 
		Polygons(ID=1) %>% 
		list %>% 
		SpatialPolygons ->
		chgrid
	
	proj4string(chgrid)<-proj4string(grid)
	pp<-SpatialPoints(coordinates(grid2),CRS(proj4string(grid2)))
	k1<-!is.na(over(pp,chgrid))
	k2<-over(pp,grid[k,])
	k2<-apply(k2,1,function(j){all(is.na(j))})
	wh<-which(k1 & k2)
	plot(pp[wh],add=TRUE,col="lightblue",cex=0.35,pch=4)
	
	
	### plot shapefiles
	plot(eu,add=TRUE,lwd=0.1,border=NA,col=alpha("grey75",0.65))
	plot(na,add=TRUE,lwd=0.1,border=NA,col=alpha("grey75",0.65))
	plot(gl,col=pb,lwd=0.5,border="grey55",add=TRUE)
	
	plot(eu,add=TRUE,lwd=0.5,border="grey55")
	plot(na,add=TRUE,lwd=0.5,border="grey55")
	
	### bathymetry lines
	plot(gUnionCascaded(b200),border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(gUnionCascaded(b1000),border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b2000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b3000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b4000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b5000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b6000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	
	### SPECIES INFO BOX
	rect(1100000,1560000,3800000,3100000,col=alpha("white",0.4),border=NA) #2260000
	rect(1100000,2680000,3800000,3100000,col=alpha("white",0.25),border=NA)
	rect(1100000,2240000,3800000,2350000,col=alpha("white",0.25),border=NA)
	rect(2450000,-1000000,3650000,1225000,col=alpha("white",0.4),border=NA)
	
	hide<-gIntersection(gr,bbox2pol(c(1100000,3800000,1560000,3100000),proj4string=laea)) # plot over the topright box to hide it but not hide cells in the west of greenland
	plot(hide,add=TRUE,border=NA,col="grey75")
	plot(gr,add=TRUE,lwd=0.5,border="grey55")
	
	o1<-over(lat,na)
	o2<-over(lat,eu)
	plot(lat[!is.na(o1) | !is.na(o2)],add=TRUE,col="grey30",pch=16,cex=0.01)
	o1<-over(lon,na)
	o2<-over(lon,eu)
	plot(lon[!is.na(o1) | !is.na(o2)],add=TRUE,col="grey30",pch=16,cex=0.01)
	
	### LEGEND DENSITY
	if(is.null(br)){
		se<-seq(r[1],r[2],length.out=7)
		lcols<-rev(alpha(tail(colo.scale(c(grid$val,se),cols=rev(cols),breaks=br),length(se)),trans))
	}else{
		se<-paste(format(round(br[-length(br)],1),nsmall=1,digits=0),format(round(br[-1],1),nsmall=1,digits=0),sep=" - ")
		lcols<-alpha(cols,trans)
	}
	deleg<-c("0",paste0(c(">",rep("",length(se)-1)),if(is.numeric(se)){round(se,0)}else{se}))
	deleg<-paste(deleg,c("/ not visited (  )",rep("",length(deleg)-1)))
	l<-legend(2500000,1100000,adj=c(0,0.5),title.adj=0,legend=rep("",length(deleg)),y.intersp=1.2,bty="n",title="Density (Birds / km\U00B2)\nDensité (Oiseaux / km\U00B2)",cex=tex*1)
	
	### add hexagonal density markers
	for(j in seq_along(l$text$x)){
		X<-l$text$x[j]
		Y<-l$text$y[j]
		shift<-c(X-coordinates(hex)[1,1],Y-coordinates(hex)[1,2])-c(400000,-7000)
		col<-c(NA,lcols)[j]
		bord<-c("lightblue",rep(NA,length(lcols)))[j]
		e<-elide(hex,shift=shift,rotate=0) ###!!!!!!!!!!!! la valeur du rotate doit être ajustée à la mitaine en fonction de la cellule choisie hex
		plot(e,col=col,add=TRUE,border=bord,lwd=0.5)
		text(coordinates(e)[,1]+100000,coordinates(e)[,2],label=deleg[j],cex=tex*1,adj=c(0,0.5))
		if(j==1){
			width<-strwidth(deleg[j],cex=tex*1)
			points(coordinates(e)[,1]+100000+width-56000,coordinates(e)[,2]-7000,pch=4,cex=0.35,col="lightblue") 
		}
	}
	sca<-bbox(e) # needed for scale position
	
	
	### CV 
	### LEGEND CV
	#####points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col="white")
	#points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=grid$cex,col="#7AAFD1")
	##points(coordinates(grid)[,1],coordinates(grid)[,2],pch=1,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col=alpha("green",0.8))
	#####points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col=alpha("black",0.5))
	se<-seq(min(grid$cv,na.rm=TRUE),max(grid$cv,na.rm=TRUE),length.out=4)
	#####se<-c(25,50,100,200,300)
	#legend(2240000,-200000,pch=1,col="lightblue",pt.cex=1.2*mag*(se/max(grid$cv,na.rm=TRUE)),y.intersp=0.75,legend=paste0(c(rep("",length(se)-1),">"),round(se,0)),bty="n",title="CV (%)",cex=tex*1.3)
	
	# ideally, we should use the next lowest CV as the lower bound because the lowest corresponds to nbsamp = 1 and it is not displayed
	
	cvleg<-strsplit(gsub("\\)|\\(|\\]|\\[","",gsub(","," - ",levels(cutcv)))," - ")
	cvleg<-c("N/A (n = 1)",sapply(cvleg,function(k){paste(gsub(" ","",format(as.numeric(k),nsmall=1,digits=0)),collapse=" - ")}))
	l<-legend(2500000,-110000,adj=c(1,0.5),title.adj=0,y.intersp=1.2,legend=rep("",length(cvleg)),bty="n",title="Coefficient of variation (%)\nCoefficient de variation (%)",cex=tex*1)
	
	for(j in seq_along(l$text$x)){
		#X<-l$text$x[j]
		Y<-l$text$y[j]
		shift<-c(X-coordinates(hex)[1,1],Y-coordinates(hex)[1,2])-c(400000,-7000)
		bord<-c("lightblue",rep(NA,length(lcols)))[j]
		width<-c(0.001,cexcv)[j]*40000 # on ajoute une explication et un "fake" buffer de 0.001 pour illustrer l'absence de trous et sa signification
		hhex<-gBuffer(SpatialPoints(coordinates(hex),proj4string=CRS(proj4string(grid))),width=width,byid=TRUE) # the buffer width value needs to be adjusted manually with the cex of the legend
		hexholes<-gIntersection(gDifference(hex,hhex),hex,byid=TRUE)
		e<-elide(hexholes,shift=shift,rotate=0)
		plot(e,col=alpha("white",trans),add=TRUE,border=NA,lwd=1)
		text(coordinates(e)[,1]+100000,coordinates(e)[,2],label=cvleg[j],cex=tex*1,adj=c(0,0.5))
	}
	
	
	### SAMPLE SIZE
	#text(coordinates(grid)[k,1],coordinates(grid)[k,2],grid$nbsamp[k],cex=tex*0.3,col=alpha("black",0.25))
	
	### LOWER CI
	#points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag,col=alpha("#7AAFD1",ifelse(is.na(grid$coll),0,1)))
	#points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag,col=grid$coll)
	
	### UPPER CI
	#points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*0.4,col=alpha("#7AAFD1",ifelse(is.na(grid$colu),0,1)))
	#points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*0.4,col=grid$colu)
	
	##text(coordinates(grid)[,1],coordinates(grid)[,2],grid$count,cex=0.2,col=alpha("black",0.3))
	###text(coordinates(grid)[,1],coordinates(grid)[,2],grid$id,cex=0.2,col=alpha("black",0.3))
	
	gg<-unlist(strsplit(group,"\\."))
	if(nchar(gg[1])==4){
		sp<-d$English[match(gg[1],d$Alpha)]
		splat<-as.character(d$Latin[match(gg[1],d$Alpha)])
		spfr<-d$French[match(gg[1],d$Alpha)]
	}else{
		sp<-gg[1]
		splat<-"  "
		spfr<-unname(groupn[match(sp,names(groupn))])
	}
	mmonth<-match(strsplit(group,"\\.")[[1]][2],month_comb)
	
	text(1600000,2930000,sp,font=2,adj=c(0,0.5),cex=tex*1.4)
	text(1600000,2780000,spfr,font=2,adj=c(0,0.5),cex=tex*1.4)
	text(3600000,2930000,unlist(strsplit(splat," "))[[1]],font=3,adj=c(1,0.5),cex=tex*1.1)
	text(3600000,2780000,unlist(strsplit(splat," "))[[2]],font=3,adj=c(1,0.5),cex=tex*1.1)
	
	text(1600000,2570000,"No. of records / Nb. de mentions :",adj=c(0,0.5),cex=tex)
	text(1600000,2450000,"Sample size / Taille d'échantillon :",adj=c(0,0.5),cex=tex)
	text(3600000,2570000,sum(dat$Count!="",na.rm=TRUE),adj=c(1,0.5),cex=tex)
	text(3600000,2450000,length(unique(dat$SMP_LABEL)),adj=c(1,0.5),cex=tex)
	
	wmonthEN<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	wmonthFR<-c("Jan","Fév","Mar","Avr","Mai","Jun","Jul","Aoû","Sep","Oct","Nov","Déc")
	nmonth<-1:12
	se<-seq(1600000,3500000,length.out=12)
	text(se,2325000,wmonthEN,col=ifelse(nmonth%in%monthNB[[mmonth]],"black","grey65"),cex=0.35,adj=c(0,0.5))
	text(se,2275000,wmonthFR,col=ifelse(nmonth%in%monthNB[[mmonth]],"black","grey65"),cex=0.35,adj=c(0,0.5))
	
	
	### barplot
	s<-unique(dl[[group]][,c("Date","SMP_LABEL","SMP_EFFORT")])
	s$Date2<-substr(s$Date,6,10)
	tab<-unlist(dlply(s,.(Date2),function(x){sum(x$SMP_EFFORT)}))
	#tab<-table(substr(s$Date,6,10))
	sc<-substr(seq.Date(as.Date(paste0("2008-",min(substr(s$Date,6,10)))),as.Date(paste0("2008-",max(substr(s$Date,6,10)))),by=1),6,10)
	miss<-setdiff(sc,names(tab))
	add<-rep(0,length(miss))
	names(add)<-miss
	tab<-c(tab,add)
	tab<-tab[order(names(tab))]
	mm<-substr(seq.Date(as.Date("2007-12-01"),as.Date("2008-11-30"),by=1),6,10)
	tab<-tab[order(match(names(tab),mm))]
	tab<-tab[1:(length(tab)-min(which(!rev(tab)==0))+1)] # tricks pour le 1 janvier ? v?rifier si p?riodes c
	
	
	s<-dl[[group]]
	s$Date2<-substr(s$Date,6,10)
	s<-ddply(s,.(Date2),function(k){sum(as.numeric(k$Count),na.rm=TRUE)})
	#tab2<-s$V1
	#names(tab2)<-s$Date2
	
	temp<-data.frame(Date2=names(tab),eff=tab,stringsAsFactors=FALSE)
	temp<-join(temp,s,type="full")
	
	
	names(tab)[setdiff(seq_along(tab),seq(1,length(tab),by=10))]<-""
	subplot({barplot(tab,las=2,cex.names=0.3,cex.lab=0.3,cex.axis=0.3,yaxt="n",border=NA,ylab="",col=db);
		axis(2,cex.axis=0.3,cex.lab=0.3,tcl=-0.1,lwd=0.1,las=2,col.axis=db);
		par(new=TRUE);
		barplot(temp$V1/temp$eff,las=2,cex.names=0.3,cex.lab=0.3,yaxt="n",cex.axis=0.3,border=NA,col=cols[length(cols)]);
		axis(4,cex.axis=0.3,cex.lab=0.3,tcl=-0.1,lwd=0.1,las=2,col.axis=cols[length(cols)]);
		mtext("Effort (km)",side=2,cex=0.3,line=0.6);
		mtext("Birds / km\nOiseaux / km",side=4,cex=0.3,line=0.5);
		mtext("Daily effort (km) and raw linear bird densities\nEffort journalier (km) et densités linéaires brutes d'oiseaux",side=1,cex=0.35,line=0.7)}
		,x=c(1600000, 3350000),y=c(1860000, 2160000)) #c(-1125000, -750000)
	
	
	### MODULE DE IC HEXAGONAL
	#cent<-2500000
	#h1<-hexpolygon(cent,-800000,dx=200000,dy=120000)
	#h2<-hexpolygon(cent,-800000,dx=200000/2,dy=120000/2)
	#h3<-hexpolygon(cent,-800000,dx=200000/4,dy=120000/4)
	#text(cent+250000,max(as.numeric(gsub("native","",h1$y))),"pred",cex=tex*0.8,adj=c(0,0.5))
	#text(cent+250000,max(as.numeric(gsub("native","",h2$y))),"lowCI",cex=tex*0.8,adj=c(0,0.5))
	#text(cent+250000,max(as.numeric(gsub("native","",h3$y))),"uppCI",cex=tex*0.8,adj=c(0,0.5))
	#h1<-SpatialPolygons(list(Polygons(list(Polygon(matrix(as.numeric(gsub("native","",c(h1$x,h1$y))),ncol=2))),ID=1)),proj4string=CRS(proj4string(grid)))
	#h2<-SpatialPolygons(list(Polygons(list(Polygon(matrix(as.numeric(gsub("native","",c(h2$x,h2$y))),ncol=2))),ID=1)),proj4string=CRS(proj4string(grid)))
	#h3<-SpatialPolygons(list(Polygons(list(Polygon(matrix(as.numeric(gsub("native","",c(h3$x,h3$y))),ncol=2))),ID=1)),proj4string=CRS(proj4string(grid)))
	#plot(h1,add=TRUE,col=alpha("red",0.5),border=NA)
	#plot(h2,add=TRUE,col=alpha("white",0.5),border=NA)
	#plot(h3,add=TRUE,col=alpha("black",0.5),border=NA)
	
	#subplot({hist(grid$diff,breaks=seq(0,300000,by=5),xlim=c(0,100))}, c(-1353981.9, 0), c(7400000, 7900000),pars=list(bg="yellow"))
	
	
	
	### LATITUDES numbers
	text(xxlat,yylat[-1],paste0(selat[-1],"°N"),xpd=TRUE,cex=tex*0.5,adj=c(-0.15,-1))
	text(xxlon[-1],yylon,gsub("-","",paste0(selon[-1],"°W")),xpd=TRUE,cex=tex*0.5,adj=c(-0.15,-1.25))
	
	off<-50000
	lines(sca[1,],rep(sca[2,1],2)-off,lwd=1)
	lines(rep(sca[1,1],2),sca[2,1]-off+c(-10000,10000),lwd=1)
	lines(rep(sca[1,2],2),sca[2,1]-off+c(-10000,10000),lwd=1)
	text(((sca[1,2]-sca[1,1])/2)+sca[1,1],sca[2,1]-off-40000,paste((sca[1,2]-sca[1,1])/1000,"km"),adj=c(0.5,0.5),cex=tex*0.5)
	
	rect(-60001100000,-70000000000,6000000000,-1290000,col=alpha("white",0.4),border=NA)
	#rect(-60001100000,2960000,6000000000,3000000000,col=alpha("white",0.4),border=NA)
	text(par("usr")[1],-1320000,"Predicted densities are derived from a distance sampling model using Distance 6.0 and the GeoAviR R package with the Eastern Canadian Seabirds-at-Sea database. Detection probabilities have been estimated by species guilds. The number of samples corresponds to the number of CruiseID/Date/Cell combinations.",cex=tex*0.4,adj=c(-0.01,0.5))
	text(par("usr")[1],-1370000,"Ces densités proviennent de modèles d'échantillonnage par distance basé sur le logiciel Distance 6.0 et le package GeoAviR et utilisant les données des Oiseaux en mer de l'est du Canada. Les probabilités de détection ont été estimés pour des groupes d'espèces similaires. La taille d'échantillon correspond au nombre de combinaisons Croisières/Dates/Cellules.",cex=tex*0.4,adj=c(-0.01,0.5))
	
	### PGRID
	#pgrid(25,cex=0.15)
	
	#box(col=alpha("white",0.4),lwd=1)
	pathim<-"C:/Users/User/Documents/SCF2016_FR/ECSASatlas/temp/ECCC_ChangementClimatique/ECCC_FIP_FRA_COUL.jpg"
	logo1 <- readJPEG(pathim,native=TRUE)
	pathim<-"C:/Users/User/Documents/SCF2016_FR/ECSASatlas/temp/Huard-SCF/Huard.png"
	logo2 <- readPNG(pathim,native=TRUE)
	
	rect(boxcut[1],boxcut[4]-80000,boxcut[1]+1000000+175000,boxcut[4],col="white",border=NA)
	rasterImage(logo1,boxcut[1],boxcut[4]-80000,boxcut[1]+1000000,boxcut[4])
	rasterImage(logo2,boxcut[1]+1000000+50000,boxcut[4]-80000,boxcut[1]+1000000+150000,boxcut[4])
	
	
	
	dev.off()
	
	### understand CI
	temp<-ml[[1]]$input_data$observations
	names(temp)[1]<-"Region"
	temp<-join(temp,dens)
	temp<-ddply(temp,.(Region),function(i){
		i$nbsample<-length(unique(i$SMP_LABEL))
		i
	})
	
}

###########################################
### CALCULATE EFFORT
###########################################


s<-unique(d[,c("cell","MonthC","SMP_LABEL","SMP_EFFORT")])

s<-ddply(s,.(cell,MonthC),function(i){
	effort<-sum(i$SMP_EFFORT)
	nbdays<-nrow(i)
	data.frame(cell=i$cell[1],MonthC=i$MonthC[1],effort,nbdays)
})

eg<-expand.grid(cell=unique(grid$id),MonthC=unique(s$MonthC))
s<-left_join(eg,s)
s$effort<-ifelse(is.na(s$effort),0,s$effort)
s$nbdays<-ifelse(is.na(s$nbdays),0,s$nbdays)

#windows()
#par(mar=c(0,0,0,0),mfrow=c(2,2))

for(i in seq_along(month_comb)[1:2]){
	
	png(paste0("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/maps/",paste0("season",month_comb[i]),".png"),width=6,height=4.8,units="in",res=500)
	
	x<-s[s$MonthC==month_comb[i],]
	
	m<-match(grid$id,x$cell)
	grid$effort<-x$effort[m]
	grid$nbdays<-x$nbdays[m]
	
	n<-6
	cols<-rev(colo.scale(seq(0,1,length.out=n),c("darkred","red","tomato3","yellow","white")))
	br<-suppressWarnings(classIntervals(unique(c(grid$effort)), n=length(cols), style = "kmeans", rtimes = 1)$brks)
	#br<-NULL
	
	grid$col<-colo.scale(grid$effort,cols=cols,breaks=br)

 ### PLOT
	par(mar=c(0,0,0,0),mgp=c(0.5,0.1,0))
	plot(grid,col="white",border="white")

	plot(b0,col=hcl(240,50,l[1]),border=NA,add=TRUE)
	plot(b200,col=hcl(240,50,l[2]),border=NA,add=TRUE)
	plot(b1000,col=hcl(240,50,l[3]),border=NA,add=TRUE)
	plot(b2000,col=hcl(240,50,l[4]),border=NA,add=TRUE)
	plot(b3000,col=hcl(240,50,l[5]),border=NA,add=TRUE)
	plot(b4000,col=hcl(240,50,l[6]),border=NA,add=TRUE)
	plot(b5000,col=hcl(240,50,l[7]),border=NA,add=TRUE)
	plot(b6000,col=hcl(240,50,l[8]),border=NA,add=TRUE)
	
	plot(grid,col=grid$col,border=ifelse(grid$effort<0.001,"lightblue",NA),lwd=0.5,add=TRUE)
	
	### bathymetry lines
	plot(gUnionCascaded(b200),border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(gUnionCascaded(b1000),border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b2000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b3000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b4000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b5000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	plot(b6000,border=alpha("black",0.2),add=TRUE,lwd=0.5)
	
	### draw latitudes
	plot(lat,add=TRUE,col="grey20",pch=16,cex=0.01)

	### plot grid and values
	plot(eu,add=TRUE,lwd=0.1,border=NA,col="grey75")
	plot(na,add=TRUE,lwd=0.1,border=NA,col="grey75")
	
	### plot shapefiles
	plot(eu,add=TRUE,lwd=0.1,border=NA,col=alpha("grey75",0.85))
	plot(na,add=TRUE,lwd=0.1,border=NA,col=alpha("grey75",0.85))
	
	plot(eu,add=TRUE,lwd=0.5,border="grey55")
	plot(na,add=TRUE,lwd=0.5,border="grey55")
	
	### SPECIES INFO BOX
	rect(1100000,2600000,3800000,3100000,col=alpha("white",0.4),border=NA)
	
	plot(eu[eu$ADMIN=="Greenland",],add=TRUE,lwd=0.1,border="grey55",col="grey75")
	
	plot(lat[!is.na(o1) | !o2],add=TRUE,col="grey30",pch=16,cex=0.01)
	
	se<-paste(round(br[-length(br)],0),round(br[-1],0),sep=" - ")
	lcols<-alpha(cols,trans)
	legend("bottomright",legend=se,pt.bg=lcols,y.intersp=1,bty="n",title="Effort (km)" ,border="lightblue",cex=tex*1,pt.cex=tex*2.5,pt.lwd=0.5,pch=22,col="lightblue")
	
	### LATITUDES numbers
	text(xxlat,yylat[-1],paste0(selat[-1],"°N"),xpd=TRUE,cex=tex*0.5,adj=c(-0.35,-1))
	
	
	mmonth<-match(month_comb[i],month_comb)
	
	text(1600000,2900000,"Effort in km",font=2,adj=c(0,0.5),cex=tex*1.4)
	text(1600000,2720000,paste0(monthEN[mmonth],"\n",monthFR[mmonth]),adj=c(0,0.5),cex=tex)
	
	
	
	dev.off()
	
}


###########################################
### OPEN DATA
###########################################


opendata<-do.call("rbind",ldens)
opendata<-subset(opendata,select=-c(Parameters))
row.names(opendata)<-1:nrow(opendata)
names(opendata)[which(names(opendata)=="Estimates")]<-"Density"

#g<-ddply(opendata,.(Region,Month),function(i){unique(i$nbsamp)})

eg<-expand.grid(cell=unique(grid$id),MonthC=unique(s$MonthC))
x<-left_join(eg,d)

### éliminer les sp
g<-ddply(x,.(cell,MonthC),function(i){
	nbspecies<-length(na.omit(unique(i$Alpha))) #éliminer les sp, UN, ALCI, espèces terrestres
	nbind<-sum(i$Count,na.rm=TRUE)
	nbobs<-sum(i$Count>0,na.rm=TRUE)
	nbdays<-length(na.omit(unique(i$Date)))
	nbsamples<-length(na.omit(unique(i$SMP_LABEL)))
	nbkm<-sum(i$SMP_EFFORT[!duplicated(i$SMP_LABEL)],na.rm=TRUE)
	nbcruiseID<-length(na.omit(unique(i$CruiseID)))
	nbships<-length(na.omit(unique(i$PlatformName)))
	data.frame(cell=i$cell[1],MonthC=i$MonthC[1],nbspecies,nbobs,nbind,nbsamples,nbkm,nbships,nbcruiseID,nbdays)
})



g<-ddply(s,.(cell,MonthC),function(i){
	effort<-sum(i$SMP_EFFORT)
	nbdays<-nrow(i)
	data.frame(cell=i$cell[1],MonthC=i$MonthC[1],effort,nbdays)
})





###########################################
### WRITE FILES FOR OPEN DATA
###########################################

write.xlsx(opendata,"C:/Users/User/Documents/SCF2016_FR/ECSASatlas/open_data.xlsx",row.names=FALSE,col.names=TRUE,sheetName="Densities",showNA=FALSE)

write.xlsx(g,"C:/Users/User/Documents/SCF2016_FR/ECSASatlas/open_data.xlsx",row.names=FALSE,col.names=TRUE,sheetName="Effort",append=TRUE,showNA=FALSE)

writeOGR(grid[,"id"],dsn="C:/Users/User/Documents/SCF2016_FR/ECSASatlas",layer="atlas_grid",driver="ESRI Shapefile")

zip("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/atlas_images",list.files("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/maps",full.names=TRUE,pattern=".png")[1:2])




# 1-Construire une fonction de détection qui est indépendente des périodes sélectionnées (donc les périodes sont lsub?)
# 2-Pour les espèces rares, utiliser le groupe pour construire la fonction de détection
# 3-

### verif dates



###########################################
### produce a plot by OBS/Mission to detect wrong patterns of detection
###########################################

keep<-ddply(d,.(Observer1,CruiseID),nrow)
keep<-ddply(d,.(Observer1,CruiseID),function(i){any(which(i$Count>1))})
cruise<-keep$CruiseID[keep$V1]
  
mobs<-distance.wrap(d[d$CruiseID%in%cruise,],#il n'y a que 4 lignes pour ce CruiseID
     SMP_EFFORT="WatchLenKm",
     DISTANCE="Distance",
     SIZE="Count",
     units=list(Type="Line",
       Distance="Perp",
       Length_units="Kilometers",
       Distance_units="Meters",
       Area_units="Square kilometers"),
     breaks=c(0,50,100,200,300), 
     estimator=list(c("HN","CO")),
     lsub=list(Observer1=NULL,CruiseID=NULL), 
     empty=c("Observer1","CruiseID"),
     split=TRUE,
     STR_AREA="STR_AREA",
     SMP_LABEL="WatchID",
     #stratum="cell",
     path="C:/Users/rousseuf/distance.wrap.output",
     pathMCDS=pathMCDS
)

names(mobs)<-gsub("-|_| ","",names(mobs)) # on dirait que les fichiers avec - _ ou des espaces ne passent pas
global.summary.distanceList(model=mobs,species=NULL,file="obs",directory="C:/Users/User/Documents")


