
library(scales)
library(sp)
library(rgeos)
library(ECSASconnect)
library(GeoAviR)
library(rgdal)
library(readxl)
library(OpenStreetMap)
library(dplyr)
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
load("C:/Users/User/Documents/SCF2016_FR/yo8.RData")


colo.scale<-function(x,cols=c("white","yellow","tomato3","darkred"),center=TRUE,alpha=1,breaks=NULL){
  w<-which(is.na(x))
  if(any(w)){
    y<-x[-w]
  }else{
    y<-x
  }
  
  re<-function(a){
    if(any(w)){
      ans<-rep(NA,length(x))
      ans[which(!is.na(x))]<-a
      ans
    }else{
      a
    }
  }
  
  if(!is.null(breaks)){
    stopifnot((length(cols)+1)==length(breaks))
    return(re(cols[as.numeric(cut(y,breaks=breaks))]))
  }

  if(length(y)==1){
    colop<-colorRampPalette(cols)
    return(re(colop(y)))
  }  
  if(class(y)=="character"){
    colop<-colorRampPalette(cols)
    color<-colop(length(unique(y)))
    return(re(color[match(y,unique(y))]))
  }else{  
    if(all(y>=0 & y<=1)){
      color<-rgb(colorRamp(cols)(y),maxColorValue=256)
      return(re(color))
    }else{
      if(any(y<0) && center){
        m<-which.max(c(abs(min(y)),max(y)))     
        sca<-0.5/ifelse(m==1,abs(min(y)),max(y))     
        xx<-sca*y+0.5
        color<-rgb(colorRamp(cols)(xx),maxColorValue=256) 
        return(re(color))
      }else{
        color<-rgb(colorRamp(cols)((max(y)-y)/(max(y)-min(y))),maxColorValue=256) 
        return(re(color))
      }
    }
  }
  
}


pathECSAS<-"C:/Users/User/Documents/SCF2016_FR/ECSASdata"
fileECSAS<-"Master ECSAS v 3.46.mdb"
groupings<-as.data.frame(read_excel("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/groupingsSOMEC.xlsx",sheet="groups"))

groupings<-as.data.frame(read_excel("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/groupingsSOMEC.xlsx",sheet="groups"))
periods<-as.data.frame(read_excel("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/groupingsSOMEC.xlsx",sheet="periods"))
periods$Start<-substr(periods$Start,6,10)
periods$End<-substr(periods$End,6,10)
#code<-as.data.frame(read_excel("U:/GeoAviR/groupingsSOMEC.xlsx",sheet="Feuil1"))$Alpha
#code<-sp[!is.na(sp)]
code<-read.csv("C:/Users/User/Documents/SCF2016_FR/ECSASdata/codes_alpha.csv")$Alpha

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

ecsas<-ECSAS.extract(sp=spcode,years=c(1800,2017),lat=c(39.33489,74.65058),
																					long = c(-90.50775,-38.75887), Obs.keep = NA, Obs.exclude = NA,
																					database = "Both", snapshot = FALSE,
																					intransect = TRUE, ecsas.drive = pathECSAS,
																					ecsas.file = fileECSAS)

#ecsas<-ECSAS.extract(sp=code[code%in%obs$Alpha],years=c(1800,2017),lat=c(39.33489,74.65058),
#  long = c(-90.50775,-38.75887), Obs.keep = NA, Obs.exclude = NA,
#  database = "Both", snapshot = FALSE,
#  intransect = TRUE, ecsas.drive = pathECSAS,
#  ecsas.file = fileECSAS)


pathMCDS<-"C:/Users/User/Documents/SCF2016_FR"
ll<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
prj<-"+proj=utm +zone=22 +datum=NAD83 +ellps=GRS80"
laea<-"+proj=laea +lat_0=50 +lon_0=-65"


### extract QC data that is not in ECSAS yet
addQC<-SOMEC2ECSAS(input="C:/Users/User/Documents/SCF2016_FR/ECSASdata/SOMEC.accdb",output="C:/Users/User/Documents/SCF2016_FR/ECSASdata/ECSASexport.csv",date="2014-04-01",step="5 min")
names(addQC)<-gsub("Orig","",names(addQC))


### replace french names
db<-odbcConnectAccess2007("C:/Users/User/Documents/SCF2016_FR/ECSASdata/SOMEC.accdb")
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
addQC$Distance<-ifelse(is.na(addQC$Distance),"",addQC$Distance) # temp car la fonction SOMEC2ECSAS retournait des NA


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
#d<-d[d$WatchLenKm>0,] #sinon distance.wrap plante
#d<-d[which(d$InTransect%in%c(-1)),] les transects vides ont des NA
d<-distance.filter(d,distance.labels=c(25,75,150,250))
d<-d[order(d$CruiseID,d$WatchID,d$Date,substr(d$StartTime,12,19)),]
d$Month<-substr(d$Date,6,7)
month_comb<-c("12010203","0405","0607","08091011")
d$MonthC<-month_comb[sapply(d$Month,function(i){
  g<-grep(i,month_comb)
  if(length(g)>1){
    if(i=="01"){g<-g[1]}
    if(i=="10"){g<-g[2]}
  }
  g
})]


eu<-readOGR(dsn="C:/Users/User/Documents/SCF2016_FR/shapefiles",layer="ne_10m_admin_0_countries",encoding="UTF-8")
eu<-eu[eu$GEOUNIT%in%c("Greenland","Iceland","United Kingdom","Ireland","France","Spain","Portugal"),]
eu<-spTransform(eu,CRS(laea))
na<-readOGR(dsn="C:/Users/User/Documents/SCF2016_FR/shapefiles",layer="ne_10m_admin_1_states_provinces",encoding="UTF-8")
na<-gIntersection(na,bbox2pol(na,ex=-1),byid=TRUE)
na<-spTransform(na,CRS(laea))


pathshp<-"C:/Users/User/Documents/SCF2016_FR/shapefiles"
b0<-spTransform(readOGR(dsn=pathshp,layer="b0",encoding="UTF-8"),CRS(laea))
b200<-spTransform(readOGR(dsn=pathshp,layer="b200",encoding="UTF-8"),CRS(laea))
b1000<-spTransform(readOGR(dsn=pathshp,layer="b1000",encoding="UTF-8"),CRS(laea))
b2000<-spTransform(readOGR(dsn=pathshp,layer="b2000",encoding="UTF-8"),CRS(laea))
b3000<-spTransform(readOGR(dsn=pathshp,layer="b3000",encoding="UTF-8"),CRS(laea))
b4000<-spTransform(readOGR(dsn=pathshp,layer="b4000",encoding="UTF-8"),CRS(laea))
b5000<-spTransform(readOGR(dsn=pathshp,layer="b5000",encoding="UTF-8"),CRS(laea))
b6000<-spTransform(readOGR(dsn=pathshp,layer="b6000",encoding="UTF-8"),CRS(laea))


dproj<-SpatialPointsDataFrame(SpatialPoints(matrix(c(d$LongStart,d$LatStart),ncol=2),CRS(ll)),data=d)
dproj<-spTransform(dproj,CRS(prj))


b<-bbox(dproj)
s<-100000
g<-GridTopology(c(b[1,1],b[2,1]),c(s,s),c(ceiling((b[1,2]-b[1,1])/s),ceiling((b[2,2]-b[2,1])/s)))
g<-SpatialGrid(g)
grid<-g
grid<-as(grid,"SpatialPolygons")
proj4string(grid)<-CRS(prj)
## hex grid
#grid2<-gBuffer(grid,width=s) #ceci plante avec un buffer sur toute la grille et je ne sais pas pourquoi
set.seed(111)
grid2<-spsample(grid,type="hexagonal",cellsize=s)
grid2<-HexPoints2SpatialPolygons(grid2)
grid<-grid2 ### change to hex grid here
grid<-SpatialPolygonsDataFrame(grid,data=data.frame(id=1:length(grid)),match.ID=FALSE)
grid$id<-paste0("g",grid$id)
o<-over(grid,dproj)
grid<-grid[apply(o,1,function(i){!all(is.na(i))}),]
grid$id<-1:nrow(grid)


map.osm<-openmap(c(80,-140),c(30,10),type="nps")
map.osm2<-openproj(map.osm,proj4string(grid))

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



######################################
### GLOBAL MODELS
######################################

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
    path="C:/Users/User/Documents/SCF2016_FR/ECSASatlas/distance.wrap.output",
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
dl<-dl[substr(names(dl),1,1)!="."]
w<-which(nbobs$nb_obs>=50) #on garde ce qui est en haut de 50
keep<-paste(nbobs$Alpha[w],nbobs$MonthC[w],sep=".")
dl<-dl[which(names(dl)%in%keep)] #on enl?ve ce qui n'a pas assez d'observations
ml<-vector(mode="list",length=length(dl))
names(ml)<-names(dl)


######################################
### GROUP MODELS
######################################

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

#########################################
### produce figures
#########################################

#grid<-spTransform(grid,CRS(laea))

cols<-rev(c("darkred","tomato3","orange","yellow","white"))
trans<-0.80
mag<-1
tex<-0.6
lgroup<-names(ml)
lgroup<-"HERG.08091011"
ldens<-vector(mode="list",length(lgroup))
names(ldens)<-lgroup
i<-1

for(i in seq_along(lgroup)){
  
  group<-lgroup[i]

  png(paste0("C:/Users/User/Documents/SCF2016_FR/ECSASatlas/maps/",gsub("\\.","_",group),"n.png"),width=6,height=4.8,units="in",res=500)

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
  #br<-NULL
  
  grid$col<-colo.scale(c(r,grid$val),cols=cols,breaks=br)[-(1:2)]
  grid$col<-alpha(ifelse(is.na(grid$cv),NA,grid$col),trans)
  grid$colu<-colo.scale(c(r,grid$u),cols=cols,breaks=br)[-(1:2)]
  grid$colu<-alpha(grid$colu,ifelse(is.na(grid$colu),1,trans))
  grid$coll<-colo.scale(c(r,grid$l),cols=cols,breaks=br)[-(1:2)]
  grid$coll<-alpha(grid$coll,ifelse(is.na(grid$coll),1,trans))
  
  par(mar=c(1,1,1,1),mgp=c(0.5,0.1,0))
  k<-!is.na(grid$val)
  plot(grid,bg="#7AAFD1",border="#7AAFD1")
  #plot(grid)
  #plot(map.osm2,xlim=bbox(grid)[1,],ylim=bbox(grid)[2,],add=TRUE)
  #plot(grid[k,],col="white",bg=alpha("lightblue",0.5),border="grey75",xlim=bbox(grid)[1,],ylim=bbox(grid)[2,],add=TRUE)
  
  l<-c(75,72,69,66,63,60,57,54) #c(70,65,60,55,50,45,42,39)
  plot(b0,col=hcl(240,50,l[1]),border=NA,add=TRUE)
  plot(b200,col=hcl(240,50,l[2]),border=NA,add=TRUE)
  plot(b1000,col=hcl(240,50,l[3]),border=NA,add=TRUE)
  plot(b2000,col=hcl(240,50,l[4]),border=NA,add=TRUE)
  plot(b3000,col=hcl(240,50,l[5]),border=NA,add=TRUE)
  plot(b4000,col=hcl(240,50,l[6]),border=NA,add=TRUE)
  plot(b5000,col=hcl(240,50,l[7]),border=NA,add=TRUE)
  plot(b6000,col=hcl(240,50,l[8]),border=NA,add=TRUE)
  
  ### draw latitudes
  m<-expand.grid(seq(-160,20,by=0.2),seq(25,85,by=5))
  p<-SpatialPoints(m,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  p<-spTransform(p,CRS(proj4string(grid)))
  plot(p,add=TRUE,col="black",pch=16,cex=0.01)
  
  xx<-par("usr")[2]
  m<-expand.grid(xx,seq(par("usr")[3],par("usr")[4],by=100))
  p<-SpatialPoints(m,proj4string=CRS(proj4string(grid)))
  p2<-spTransform(p,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  r<-range(coordinates(p2)[,2])
  r<-5*round(r/5) 
  se<-seq(r[1],r[2],by=5)
  yy<-sapply(se,function(k){
  	coordinates(p)[which.min(abs(coordinates(p2)[,2]-k)),2]	
  })
  text(xx,yy,paste0(se,"°N"),xpd=TRUE,cex=tex*0.5,adj=c(1.5,0.5))
  
  
  ### plot shapefiles
  plot(eu,add=TRUE,lwd=0.1,border=NA,col="grey75")
  plot(na,add=TRUE,lwd=0.1,border=NA,col="grey75")
 
  plot(grid[k,],col=grid$col[k],border=ifelse(is.na(grid$col[k]),alpha("lightblue",trans),NA),add=TRUE,lwd=0.75)
  
  plot(eu,add=TRUE,lwd=0.5,border="grey55")
  plot(na,add=TRUE,lwd=0.5,border="grey55")
  
  ### bathymetry lines
  plot(gUnionCascaded(b200),border=alpha("black",0.1),add=TRUE,lwd=0.5)
  plot(gUnionCascaded(b1000),border=alpha("black",0.1),add=TRUE,lwd=0.5)
  plot(b2000,border=alpha("black",0.1),add=TRUE,lwd=0.5)
  plot(b3000,border=alpha("black",0.1),add=TRUE,lwd=0.5)
  plot(b4000,border=alpha("black",0.1),add=TRUE,lwd=0.5)
  plot(b5000,border=alpha("black",0.1),add=TRUE,lwd=0.5)
  plot(b6000,border=alpha("black",0.1),add=TRUE,lwd=0.5)
  
 
  #rect(2000000,-2200000,3800000,3000000,col=alpha("white",0.2),border=NA)
  #rect(-2000000,2700000,4000000,3000000,col=alpha("white",0.2),border=NA)
  
  if(is.null(br)){
    se<-seq(r[1],r[2],length.out=7)
    lcols<-rev(alpha(tail(colo.scale(c(grid$val,se),cols=rev(cols),breaks=br),length(se)),trans))
  }else{
  	 se<-paste(round(br[-length(br)],1),round(br[-1],1),sep=" - ")
  	 lcols<-alpha(cols,trans)
  }
  legend(2200000,900000,fill=c(alpha(NA,trans),lcols),legend=c("0",paste0(c(">",rep("",length(se)-1)),if(is.numeric(se)){round(se,0)}else{se},c(rep("",length(se)-1),"+"))),y.intersp=0.75,bty="n",title="Bird Density\n(nb/km2)",border="lightblue",cex=tex*1.3,pt.cex=tex*2.5,pt.lwd=0.2)
  
  ### CV 
  #points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col="white")
  points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col=alpha("#7AAFD1",1))
  #points(coordinates(grid)[,1],coordinates(grid)[,2],pch=16,cex=mag*(grid$cv/max(grid$cv,na.rm=TRUE)),col=alpha("black",0.5))
  se<-seq(min(grid$cv,na.rm=TRUE),max(grid$cv,na.rm=TRUE),length.out=4)
  #se<-c(25,50,100,200,300)
  legend(2240000,-200000,pch=1,col="lightblue",pt.cex=1.2*mag*(se/max(grid$cv,na.rm=TRUE)),y.intersp=0.75,legend=paste0(c(rep("",length(se)-1),">"),round(se,0)),bty="n",title="CV (%)",cex=tex*1.3)
  
  ### n=1
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
  text(2300000,2950000,sp,font=2,adj=c(0,1),cex=tex*1.4)
  text(2300000,2780000,paste("No. obs:",nbobs$nb_obs[m]),adj=c(0,1),cex=tex*1.3)
  text(2300000,2630000,paste("No. samples:",nbobs$nb_sample[m]),adj=c(0,1),cex=tex*1.3)
  text(2300000,2480000,group,adj=c(0,1),cex=tex*1.3)

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
  
  
  
  names(tab)[setdiff(seq_along(tab),seq(1,length(tab),by=4))]<-""
  subplot({barplot(tab,las=2,cex.names=0.3,cex.lab=0.3,cex.axis=0.3,yaxt="n",border=NA,ylab="Effort (km)",col="lightblue");
  	        axis(2,cex.axis=0.3,cex.lab=0.3,tcl=-0.1,lwd=0.1,las=2,col.axis="lightblue");
  	        par(new=TRUE);
  								 barplot(temp$V1/temp$eff,las=2,cex.names=0.3,cex.lab=0.3,yaxt="n",cex.axis=0.3,border=NA,col="darkred");
  	        axis(4,cex.axis=0.3,cex.lab=0.3,tcl=-0.1,lwd=0.1,las=2,col.axis="darkred");
  	        mtext("Nb ind. / km",side=4,cex=0.3,line=0.2)}
  								,x=c(1450000, 3100000),y=c(1600000, 2100000))
  
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
  
  box(col="grey50")
  

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


#######
#######
#######
opendata<-do.call("rbind",ldens)
opendata<-subset(opendata,select=-c(Parameters))
row.names(opendata)<-1:nrow(opendata)
names(opendata)[which(names(opendata)=="Estimates")]<-"Density"



# 1-Construire une fonction de détection qui est indépendente des périodes sélectionnées (donc les périodes sont lsub?)
# 2-Pour les espèces rares, utiliser le groupe pour construire la fonction de détection
# 3-

### verif dates


#######################################################################
### produce a plot by OBS/Mission to detect wrong patterns of detection
#######################################################################

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


