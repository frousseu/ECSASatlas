
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
#load("M:/SCF2016_FR/yo9.RData")


###########################################
### INIT
###########################################


pathECSAS<-"Y:/Inventaires/Pélagiques/ECSAS"
fileECSAS<-"Master ECSAS v 3.51.mdb"
groupings<-as.data.frame(read_excel("M:/SCF2016_FR/ECSASatlas/groupingsSOMEC.xlsx",sheet="groups"))


groupings<-as.data.frame(read_excel("M:/SCF2016_FR/ECSASatlas/groupingsSOMEC.xlsx",sheet="groups"))
periods<-as.data.frame(read_excel("M:/SCF2016_FR/ECSASatlas/groupingsSOMEC.xlsx",sheet="periods"))
periods$Start<-substr(periods$Start,6,10)
periods$End<-substr(periods$End,6,10)


### get ECSAS database
db<-odbcConnectAccess2007(paste(pathECSAS,fileECSAS,sep="/"))
obs<-sqlFetch(db,"tblSighting",as.is=TRUE) #?a plante et ne sait pas pourquoi
sp<-sqlFetch(db,"tblSpeciesInfo",as.is=TRUE) #?a plante et ne sait pas pourquoi
odbcClose(db)
spcode<-table(sp$Alpha[match(obs$SpecInfoID,sp$SpecInfoID)])
spcode<-spcode[spcode>5] # temporaire, car il y a une limitation à 255 codes dans access
spcode<-names(spcode)
spcode<-spcode[!is.na(spcode)]
spcode<-spcode[sp$Class[match(spcode,sp$Alpha)]=="Bird"]
spcode<-spcode[!spcode%in%c("mowa")]
ecsas<-ECSAS.extract(sp=spcode,lat=c(39.33489,74.65058),long=c(-90.50775,-38.75887),database=c("All"),intransect=TRUE,ecsas.drive=pathECSAS,ecsas.file=fileECSAS)
### Both needs to be used with the current version dfifield cause All subsets fewer observations


### path pour MCDS.exe
pathMCDS<-"M:/SCF2016_FR" #path pour MCDS.exe


### différentes projections utilisées
ll<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
prj<-"+proj=utm +zone=22 +datum=NAD83 +ellps=GRS80"
laea<-"+proj=laea +lat_0=50 +lon_0=-65"


### extract QC data that is not in ECSAS yet
addQC<-SOMEC2ECSAS(input="M:/SCF2016_FR/ECSASdata/SOMEC.accdb",output="M:/SCF2016_FR/ECSASdata/ECSASexport.csv",date="2014-04-01",step="5 min")#,spNA=FALSE)
names(addQC)<-gsub("Orig","",names(addQC))


### Replace french names and wrong names in Quebec data
db<-odbcConnectAccess2007("M:/SCF2016_FR/ECSASdata/SOMEC.accdb")
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
#addQC$Distance<-ifelse(is.na(addQC$Distance),"",addQC$Distance) # temp car la fonction SOMEC2ECSAS retournait des NA


### build complete database
d<-ecsas
d<-join(ecsas,addQC,type="full")
#d$Date<-substr(d$Date,1,10)
d$Alpha<-ifelse(is.na(d$Alpha) | d$Alpha%in%c("RIEN","NOBI"),"",as.character(d$Alpha))
d$Distance<-ifelse(is.na(d$Distance),"",as.character(d$Distance))
dl<-c("A","B","C","D","c")
dn<-c("25","75","150","250","150")
m<-match(d$Distance,dl)
d$Distance<-ifelse(is.na(m),d$Distance,dn[m])
d<-d[which(d$LongStart>(-150) & d$LongStart<(-18)),] ### j'enl?ve la croisi?re partant vers l'uerope car cr?er des distortions dans les projections
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


### get land shapefiles
## comes form natural earth
eu<-readOGR(dsn="M:/SCF2016_FR/shapefiles",layer="ne_10m_admin_0_countries",encoding="UTF-8")
eu<-eu[eu$GEOUNIT%in%c("Greenland","Iceland","United Kingdom","Ireland","France","Spain","Portugal"),]
eu<-spTransform(eu,CRS(laea))
gr<-eu[eu$ADMIN=="Greenland",]
na<-readOGR(dsn="M:/SCF2016_FR/shapefiles",layer="ne_10m_admin_1_states_provinces",encoding="UTF-8")
na<-na[na$admin%in%c("United States of America","Canada"),]
na<-gIntersection(na,bbox2pol(na,ex=-1),byid=TRUE)
na<-spTransform(na,CRS(laea))
boxcut<-c(-1848241,3712932,-1413652,3035287) ####!!!!!! On coupe avec cette bbox (déterminée en produisant les pngs avec l'objet boxcut) pour éviter les problèmes de transparence liés au dépassement des polygones des cadres
na<-gIntersection(na,bbox2pol(boxcut,ex=-1,proj4string=laea),byid=TRUE)
eu<-gIntersection(eu,bbox2pol(boxcut,ex=-1,proj4string=laea),byid=TRUE)
gr<-gIntersection(gr,bbox2pol(boxcut,ex=-1,proj4string=laea),byid=TRUE)
gl<-readOGR(dsn="M:/SCF2016_FR/shapefiles",layer="ne_10m_lakes",encoding="UTF-8")
gl<-gl[gl$name%in%c("Lake Ontario","Lake Huron","Lake Erie","Lake Michigan","Lake Superior"),]
gl<-spTransform(gl,CRS(laea))


pathshp<-"M:/SCF2016_FR/shapefiles"
b0<-spTransform(readOGR(dsn=pathshp,layer="b0",encoding="UTF-8"),CRS(laea))
b200<-spTransform(readOGR(dsn=pathshp,layer="b200",encoding="UTF-8"),CRS(laea))
b1000<-spTransform(readOGR(dsn=pathshp,layer="b1000",encoding="UTF-8"),CRS(laea))
b2000<-spTransform(readOGR(dsn=pathshp,layer="b2000",encoding="UTF-8"),CRS(laea))
b3000<-spTransform(readOGR(dsn=pathshp,layer="b3000",encoding="UTF-8"),CRS(laea))
b4000<-spTransform(readOGR(dsn=pathshp,layer="b4000",encoding="UTF-8"),CRS(laea))
b5000<-spTransform(readOGR(dsn=pathshp,layer="b5000",encoding="UTF-8"),CRS(laea))
b6000<-spTransform(readOGR(dsn=pathshp,layer="b6000",encoding="UTF-8"),CRS(laea))





### BUILD GRID
dproj<-SpatialPointsDataFrame(SpatialPoints(matrix(c(d$LongStart,d$LatStart),ncol=2),CRS(ll)),data=d)
dproj<-spTransform(dproj,CRS(laea))
grid<-hexgrid(dproj,width=100000,convex=FALSE,seed=111)
grid2<-hexgrid(bbox2pol(dproj),width=100000,convex=FALSE,seed=111) # a second "complete" grid to find out where are unvisited cells
#b<-bbox(dproj)
#s<-100000
#g<-GridTopology(c(b[1,1],b[2,1]),c(s,s),c(ceiling((b[1,2]-b[1,1])/s),ceiling((b[2,2]-b[2,1])/s)))
#g<-SpatialGrid(g)
#grid<-g
#grid<-as(grid,"SpatialPolygons")
#proj4string(grid)<-CRS(prj)
## hex grid
#grid2<-gBuffer(grid,width=s) #ceci plante avec un buffer sur toute la grille et je ne sais pas pourquoi
#set.seed(111)
#grid2<-spsample(grid,type="hexagonal",cellsize=s)
#grid2<-HexPoints2SpatialPolygons(grid2)
#grid<-grid2 ### change to hex grid here
#grid<-SpatialPolygonsDataFrame(grid,data=data.frame(id=1:length(grid)),match.ID=FALSE)
grid$id<-paste0("g",grid$id)
#o<-over(grid,dproj)
#grid<-grid[apply(o,1,function(i){!all(is.na(i))}),]
#grid$id<-1:nrow(grid)



dshp<-SpatialPointsDataFrame(SpatialPoints(matrix(c(d$LongStart,d$LatStart),ncol=2),CRS(ll)),data=d)
o<-over(spTransform(dshp,CRS(proj4string(grid))),grid)
d$STR_LABEL<-o$id
d$cell<-o$id
d<-d[!is.na(d$STR_LABEL),]
d$Group<-groupings$Group2[match(d$Alpha,groupings$Alpha)]
d$Group<-ifelse(is.na(d$Group),"",d$Group)
d$Group2<-groupings$Group1[match(d$Alpha,groupings$Alpha)]
d$Group2<-ifelse(is.na(d$Group2),"",d$Group2)
d$Group3<-groupings$Group3[match(d$Alpha,groupings$Alpha)]
d$Group3<-ifelse(is.na(d$Group3),"",d$Group3)
d$Group<-d$Group3


### CHECK HERE IF IT'S STILL USEFULL
### compute number of observations per groups/periods
d2<-ddply(d[!is.na(d$Group),],.(Group),function(i){
   temp<-periods[which(periods$Group%in%i$Group[1]),]
   period_list<-temp$Period
   x<-d[d$Group%in%i$Group[1],] 
   w<-sapply(substr(x$Date,6,10),function(a){
     which(temp$Start<=a & temp$End>=a)   
   })
   x$Period<-temp$Period[w]
   x
})
ddply(d2,.(Alpha,Period),nrow)
ddply(d2,.(Group,Period),nrow)




###########################################
### GLOBAL MODELS
###########################################

### run model without empty transect to only get detection probability and the maximum number of observations compared to 32767

group_list<-unique(d$Group) 
group_list<-group_list[group_list!=""]


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
s<-sample(unique(d$SMP_LABEL[d$Alpha!="  "]),1000,replace=FALSE)
#s<-sample(setdiff(d$WatchID,unique(d$WatchID[d$Group==""])),3000,replace=FALSE)
ds<-d[d$SMP_LABEL%in%s,]   # ? 32768 SMP_LABEL, MCDS plante 32768
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
    lsub=list(Group=group_list), 
    empty=NULL,
    split=TRUE,
    STR_AREA="STR_AREA",
    SMP_LABEL="SMP_LABEL",
    STR_LABEL="STR_LABEL",
    path="M:/SCF2016_FR/ECSASatlas/distance.wrap.output",
    pathMCDS=pathMCDS
)




### number of watches and observations for each group
nbobs<-ddply(d,.(Alpha,MonthC),function(x){
  nb_sample<-length(unique(d$SMP_LABEL[d$MonthC==x$MonthC[1]]))
  nb_obs<-sum(x$Alpha!="")
  c(nb_sample,nb_obs)
})
names(nbobs)[1:4]<-c("Alpha","MonthC","nb_sample","nb_obs")
nbobs<-nbobs[nbobs$Alpha!="",]

### number of cells for each MonthC
ddply(d,.(MonthC),function(x){
  length(unique(x$cell))
})

###
mult_list<-sapply(group_list,function(i){
  x<-mg[[i]]$parameter_estimates$Global
  x$Estimates[x$Parameters=="p"]
})

mult_list<-mult_list[match(d$Group[match(unique(d$Alpha),d$Alpha)],names(mult_list))]
names(mult_list)<-unique(d$Alpha)
mult_list<-mult_list[!is.na(mult_list)]
mult_list<-c(mult_list,ABCD=mean(mult_list)) ### added the mean for doing on all species

### build data for each species
dl<-dlply(d,.(Alpha,MonthC),function(x){ # data list
  y<-d[d$MonthC==x$MonthC[1],]
  w1<-which(y$Alpha!=x$Alpha[1])
  y$Empty<-0
  y$Empty[w1]<-1
  y$Alpha[w1]<-""
  y$Count[w1]<-""
  y<-y[order(y$SMP_LABEL,y$Empty),]
  y[which(y$Empty==0 | !duplicated(y$SMP_LABEL)),]
})
### build data for all species combined
d2<-d
d2$Alpha<-ifelse(d$Alpha=="",d$Alpha,"ABCD")
dl2<-dlply(d2,.(Alpha,MonthC),function(x){ # data list
	y<-d2[d2$MonthC==x$MonthC[1],]
	w1<-which(y$Alpha!=x$Alpha[1])
	y$Empty<-0
	y$Empty[w1]<-1
	y$Alpha[w1]<-""
	y$Count[w1]<-""
	y<-y[order(y$SMP_LABEL,y$Empty),]
	y[which(y$Empty==0 | !duplicated(y$SMP_LABEL)),]
})

### subset list and combine with all species
dl<-c(dl,dl2)
dl<-dl[substr(names(dl),1,1)!="."]
w<-which(nbobs$nb_obs>=50) #on garde ce qui est en haut de 50
keep<-c(paste(nbobs$Alpha[w],nbobs$MonthC[w],sep="."),names(dl2))
dl<-dl[which(names(dl)%in%keep)] #on enl?ve ce qui n'a pas assez d'observations
ml<-vector(mode="list",length=length(dl))
names(ml)<-names(dl)

### ADD A TOATL


###########################################
### GROUP MODELS
###########################################

#ALSP.08091011 ne run pas pour une raison obscure

for(i in seq_along(dl)){
  
   x<-dl[[i]]
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
     path="M:/SCF2016_FR/ECSASatlas/distance.wrap.output",
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


#grid2<-spTransform(grid2,CRS(laea))

#grid<-spTransform(grid,CRS(laea))
#reg_atl<-readOGR("M:/SCF2016_FR/shapefiles",layer="ATLBioregions",verbose=FALSE)
#reg_atl<-spTransform(reg_atl,CRS(proj4string(grid)))


hex<-grid[1,]
row.names(hex@data)<-sapply(slot(hex, "polygons"), function(x) slot(x, "ID"))

cols<-rev(c("darkred",colo.scale(seq(0,1,length.out=3),c("red","white"))))
trans<-0.65
mag<-1
tex<-0.6
monthEN<-c("December to March","April to July","August to November")
monthFR<-c("Décembre à Mars","Avril à Juillet","Août à Novembre")
monthNB<-list(c(12,1:3),4:7,8:11)
#lgroup<-names(ml)
lgroup<-c("DOVE.08091011")
ldens<-vector(mode="list",length(lgroup))
names(ldens)<-lgroup
i<-1

for(i in seq_along(lgroup)){
  
  group<-lgroup[i]
  
  if(class(ml[[group]])=="character"){ # this is for models that do not run
  	next
  }

  png(paste0("M:/SCF2016_FR/ECSASatlas/maps/",gsub("\\.","_",group),"_.png"),width=6,height=4.8,units="in",res=500)

  dens<-density.map(ml[[group]],by.stratum=TRUE)
  temp<-ddply(dl[[group]],.(cell),function(k){length(unique(k$SMP_LABEL))})
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

  br<-suppressWarnings(classIntervals(unique(c(grid$val)), n=length(cols), style = "kmeans", rtimes = 1)$brks)
  br2<-suppressWarnings(classIntervals(unique(c(grid$val)), n=length(cols), style = "quantile", rtimes = 1)$brks) #put this on graph to study quantile compared to kmeans
  #br<-NULL
  
  grid$col<-colo.scale(c(r,grid$val),cols=cols,breaks=br)[-(1:2)]
  grid$col<-alpha(ifelse(is.na(grid$cv),NA,grid$col),trans)
  grid$colu<-colo.scale(c(r,grid$u),cols=cols,breaks=br)[-(1:2)]
  grid$colu<-alpha(grid$colu,ifelse(is.na(grid$colu),1,trans))
  grid$coll<-colo.scale(c(r,grid$l),cols=cols,breaks=br)[-(1:2)]
  grid$coll<-alpha(grid$coll,ifelse(is.na(grid$coll),1,trans))
  ncv<-4
  brcv<-suppressWarnings(classIntervals(unique(grid$cv), n=ncv, style = "kmeans", rtimes = 1)$brks)
  cexcv<-seq(0.2,1,length.out=4)
  cutcv<-cut(grid$cv,breaks=brcv)
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
  
  grid[k,] %>% 
  	gUnaryUnion %>% 
  	slot("polygons") %>% 
  	extract2(1) %>% 
  	slot("Polygons") %>%
  	extract(keep) %>% 
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
  rect(1100000,2250000,3800000,2350000,col=alpha("white",0.25),border=NA)
  
  hide<-gIntersection(gr,bbox2pol(c(1100000,3800000,1560000,3100000),proj4string=laea)) # plot over the topright box to hide it but not hide cells in the west of greenland
  plot(hide,add=TRUE,border=NA,col="grey75")
  plot(gr,add=TRUE,lwd=0.5,border="grey55")
  
  o1<-over(lat,na)
  o2<-over(lat,eu)
  plot(lat[!is.na(o1) | !is.na(o2)],add=TRUE,col="grey30",pch=16,cex=0.01)
  
	  ### LEGEND DENSITY
	  if(is.null(br)){
	    se<-seq(r[1],r[2],length.out=7)
	    lcols<-rev(alpha(tail(colo.scale(c(grid$val,se),cols=rev(cols),breaks=br),length(se)),trans))
	  }else{
	  	 se<-paste(format(round(br[-length(br)],1),nsmall=1,digits=0),format(round(br[-1],1),nsmall=1,digits=0),sep=" - ")
	  	 se2<-paste(round(br2[-length(br2)],1),round(br2[-1],1),sep=" - ") #illustrate results with quantiles instead
	  	 lcols<-alpha(cols,trans)
	  }
   deleg<-c("0",paste0(c(">",rep("",length(se)-1)),if(is.numeric(se)){round(se,0)}else{se}))
   deleg<-paste(deleg,c("/ not visited (  )",rep("",length(deleg)-1)))
   deleg2<-c("0",paste0(c(">",rep("",length(se2)-1)),if(is.numeric(se2)){round(se2,0)}else{se2})) #show quantile instead
   l<-legend(2500000,1100000,adj=c(0,0.5),title.adj=0,legend=rep("",length(deleg)),y.intersp=1.2,bty="n",title="Density(Birds / Km\U00B2)\nDensité (Oiseaux / Km\U00B2)",cex=tex*1)
	  
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
	    text(coordinates(e)[,1]-100000,coordinates(e)[,2],label=deleg2[j],cex=tex*1,adj=c(1,0.5),col="lightblue")
	    if(j==1){
	    	 width<-strwidth(deleg[j],cex=tex*1)
	      points(coordinates(e)[,1]+100000+width-56000,coordinates(e)[,2]-7000,pch=4,cex=0.35,col="lightblue") 
	    }
	  }
	  
	  
	  
	  ### CV 
	  ### LEGEND CV
	  #####points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col="white")
  #points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=grid$cex,col="#7AAFD1")
  ##points(coordinates(grid)[,1],coordinates(grid)[,2],pch=1,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col=alpha("green",0.8))
  #####points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col=alpha("black",0.5))
  se<-seq(min(grid$cv,na.rm=TRUE),max(grid$cv,na.rm=TRUE),length.out=4)
  #####se<-c(25,50,100,200,300)
  #legend(2240000,-200000,pch=1,col="lightblue",pt.cex=1.2*mag*(se/max(grid$cv,na.rm=TRUE)),y.intersp=0.75,legend=paste0(c(rep("",length(se)-1),">"),round(se,0)),bty="n",title="CV (%)",cex=tex*1.3)
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
  
  m<-match(group,paste(nbobs$Alpha,nbobs$Month,sep="."))
  sp<-d$English[match(nbobs$Alpha[m],d$Alpha)]
  splat<-d$Latin[match(nbobs$Alpha[m],d$Alpha)]
  spfr<-qc$NomFR[match(nbobs$Alpha[m],qc$CodeAN)]
  
  mmonth<-match(strsplit(group,"\\.")[[1]][2],month_comb)
  
  text(3600000,2930000,sp,font=2,adj=c(1,0.5),cex=tex*1.4)
  text(3600000,2780000,spfr,font=2,adj=c(1,0.5),cex=tex*1.4)
  #text(3600000,,paste0(monthEN[mmonth]," / ",monthFR[mmonth]),adj=c(1,0.5),cex=tex)
  text(3600000,2570000,paste("No. obs. / Nb obs. :  ",nbobs$nb_obs[m]),adj=c(1,0.5),cex=tex)
  text(3600000,2450000,paste("No. samples / Taille d'échantillon :  ",nbobs$nb_sample[m]),adj=c(1,0.5),cex=tex)
 
  wmonth<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  nmonth<-1:12
  se<-seq(1600000,3450000,length.out=12)
  text(se,2300000,wmonth,col=ifelse(seq_along(wmonth)%in%monthNB[[mmonth]],"black","grey55"),cex=0.35)

  
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
  	        mtext("Effort (Km)",side=2,cex=0.3,line=0.6);
  	        mtext("Birds / km\nOiseaux / Km",side=4,cex=0.3,line=0.5);
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
  text(xxlat,yylat[-1],paste0(selat[-1],"°N"),xpd=TRUE,cex=tex*0.5,adj=c(-0.35,-1))
  
  rect(-60001100000,-70000000000,6000000000,-1290000,col=alpha("white",0.4),border=NA)
  #rect(-60001100000,2960000,6000000000,3000000000,col=alpha("white",0.4),border=NA)
  text(par("usr")[1],-1320000,"Predicted densities are derived from a distance sampling model using Distance 6.0 and the GeoAviR R package with the Eastern Canadian Seabirds-at-Sea database. Detection probabilities have been estimated by species guilds. The number of samples corresponds to the number of CruiseID/Date/Cell combinations.",cex=tex*0.4,adj=c(-0.01,0.5))
  text(par("usr")[1],-1370000,"Ces densités proviennent de modèles d'échantillonnage par distance basé sur le logiciel Distance 6.0 et le package GeoAviR et utilisant les données des Oiseaux en mer de l'est du Canada. Les probabilités de détection ont été estimés pour des groupes d'espèces similaires. La taille d'échantillon correspond au nombre de combinaisons Croisières/Dates/Cellules.",cex=tex*0.4,adj=c(-0.01,0.5))
  
  ### PGRID
  #pgrid(25,cex=0.15)
  
  #box(col=alpha("white",0.4),lwd=1)

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
	
	png(paste0("M:/SCF2016_FR/ECSASatlas/maps/",paste0("season",month_comb[i]),".png"),width=6,height=4.8,units="in",res=500)
	
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
	
	text(1600000,2900000,"Effort in Km",font=2,adj=c(0,0.5),cex=tex*1.4)
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

write.xlsx(opendata,"M:/SCF2016_FR/ECSASatlas/open_data.xlsx",row.names=FALSE,col.names=TRUE,sheetName="Densities",showNA=FALSE)

write.xlsx(g,"M:/SCF2016_FR/ECSASatlas/open_data.xlsx",row.names=FALSE,col.names=TRUE,sheetName="Effort",append=TRUE,showNA=FALSE)

writeOGR(grid[,"id"],dsn="M:/SCF2016_FR/ECSASatlas",layer="atlas_grid",driver="ESRI Shapefile")

zip("M:/SCF2016_FR/ECSASatlas/atlas_images",list.files("M:/SCF2016_FR/ECSASatlas/maps",full.names=TRUE,pattern=".png")[1:2])




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
global.summary.distanceList(model=mobs,species=NULL,file="obs",directory="M:/")


