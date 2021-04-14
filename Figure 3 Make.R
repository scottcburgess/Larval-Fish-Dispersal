### This code produces Figure 3 in the manuscript: 
### Burgess SC, Bode M, Leis JM, Mason LB. Selection on marine larval-fish swimming speed and the emergence of dispersal kernels. Submitted for publication.

library(reshape) # for melt
library(viridis) # for plasma color palette
library(plotrix) # for draw.circle
library(extrafont)
loadfonts(device="postscript")

# setwd("")
load("d_Inshore_Swimming1000.RData")
load("d_Inshore_Passive1000.RData")

ind <- 1:length(d_Inshore_Swimming1000)


# Get distribution of predicted dispersal distances per degree of latitude
coords <- seq(from=-500, to=500, by=10) #coords

### Swimming model
# m is a matrix with xcoords as columns and ycoords as rows
m<-matrix(NA, ncol=length(coords), nrow=length(coords))

colnames(m)<-coords #xcoords
row.names(m)<-coords #ycoords

 # For loop to fill in the matrix 
# j is the data frame with the model predictions
dat <- do.call(rbind.data.frame,d_Inshore_Swimming1000)
j <- dat[dat$Settlement==1,]

# Choose certain island groups
# Percy's
# j <- j[j$Start.point.lat>-21.9 & j$Start.point.lat < (-21.6) & j$Start.point.long > 150 & j$Start.point.long < 151,]
# # with(j,plot(Start.point.long,Start.point.lat))
# Keppel
# j <- j[j$Start.point.lat>-23.23 & j$Start.point.lat < (-23) & j$Start.point.lon > 150.85 & j$Start.point.lon < 151.03,] 
# # with(j,plot(Start.point.long,Start.point.lat))
# Cap Bunk
# j <- j[j$Start.point.lat>-23.94 & j$Start.point.lat < (-23.1) & j$Start.point.lon > 151.6 & j$Start.point.lon < 152.45,] 
# # with(j,plot(Start.point.long,Start.point.lat))

foo <- j
foohist <- with(j,hist(Distance.km,breaks=seq(0,500,10),plot=F))

for (i in 2:ncol(m)){  
  # nd is the subset of the data frame (j) that includes xcoords in the interval of columns i and i-1
  # than xcoords of column i-1
  nd<-j[j$xcoord <as.numeric(colnames(m)[i]) & j$xcoord>(as.numeric(colnames(m)[i-1])),]
  # nr<-nrow(nd)  
  for (k in 2:nrow(m)) {
    #nd2 is the subset of nd that has ycoords values in the interval of row names k and k-1
    nd2<-nd[nd$ycoord <as.numeric(row.names(m)[k]) & nd$ycoord>(as.numeric(row.names(m)[k-1])),]    
    if (nrow(nd2)>0){
      # m[k,i] is the proportion of observations (rows of data) in the xcoord interval i-1 to i that
      # have ycoord distances in the interval k-1 to k
      # m[k,i]<-nrow(nd2)/nrow(nd)
      m[k,i]<-nrow(nd2)/nrow(j[j$Settlement==1,])
    }
    else {
      m[k,i]<-0
    }    
  }
  print(i)
}
m2 <- m/max(m,na.rm=T)
# library(reshape)
# the function melt() converts the matrix into a data frame
M<-melt(m)
M<-M[complete.cases(M),]
colnames(M)<-c('ycoord', 'xcoord', 'proportion')

M2<-melt(m2)
M2<-M2[complete.cases(M2),]
colnames(M2)<-c('ycoord', 'xcoord', 'proportion')


### Passive model
# mp is a matrix with xcoords as columns and ycoords as rows
mp<-matrix(NA, ncol=length(coords), nrow=length(coords))

colnames(mp)<-coords #xcoords
row.names(mp)<-coords #ycoords

 # For loop to fill in the matrix 
# j is the data frame with the model predictions
dat.passive <- do.call(rbind.data.frame,d_Inshore_Passive1000)
j <- dat.passive[dat.passive$Settlement==1,]

# Choose certain island groups
# Percy
# j <- j[j$Start.point.lat>-21.9 & j$Start.point.lat < (-21.6) & j$Start.point.long > 150 & j$Start.point.long < 151,] 
# # with(j,plot(Start.point.long,Start.point.lat))
# Keppel
# j <- j[j$Start.point.lat>-23.23 & j$Start.point.lat < (-23) & j$Start.point.lon > 150.85 & j$Start.point.lon < 151.03,]  
# # with(j,plot(Start.point.long,Start.point.lat))
# Cap Bunk
# j <- j[j$Start.point.lat>-23.94 & j$Start.point.lat < (-23.1) & j$Start.point.lon > 151.6 & j$Start.point.lon < 152.45 ,]
# # with(j,plot(Start.point.long,Start.point.lat))

foop <- j
foophist <- with(j,hist(Distance.km,breaks=seq(0,500,10),plot=F))

for (i in 2:ncol(m)){  
  # nd is the subset of the data frame (j) that includes xcoords in the interval of columns i and i-1
  # than latitude of column i-1
  nd<-j[j$xcoord <as.numeric(colnames(m)[i]) & j$xcoord>(as.numeric(colnames(m)[i-1])),]
  # nr<-nrow(nd)  
  for (k in 2:nrow(m)) {
    #nd2 is the subset of nd that has ycoords values in the interval of row names k and k-1
    nd2<-nd[nd$ycoord <as.numeric(row.names(m)[k]) & nd$ycoord>(as.numeric(row.names(m)[k-1])),]    
    if (nrow(nd2)>0){
      # mp[k,i] is the proportion of observations (rows of data) in the xcoord interval i-1 to i that
      # have ycoord distances in the interval k-1 to k
      # m[k,i]<-nrow(nd2)/nrow(nd)
      mp[k,i]<-nrow(nd2)/nrow(j[j$Settlement==1,])
    }
    else {
      mp[k,i]<-0
    }    
  }
  print(i)
}
m2p <- mp/max(mp,na.rm=T)

# the function melt() converts the matrix into a data frame
Mp<-melt(mp)
Mp<-Mp[complete.cases(Mp),]
colnames(Mp)<-c('ycoord', 'xcoord', 'proportion')

M2p<-melt(m2p)
M2p<-M2p[complete.cases(M2p),]
colnames(M2p)<-c('ycoord', 'xcoord', 'proportion')


# Function to plot color bar
color.bar <- function(lut, min, max=-min, nticks=3, ticks=seq(min, max, len=nticks), title='') {
    scale = (length(lut)-1)/(max-min)

    # dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
    axis(2, ticks, las=1)
    for (i in 1:(length(lut)-1)) {
     y = (i-1)/scale + min
     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}


quartz(width=7,height=7)
cexl <- 1.2
par(oma=c(1,4,1,1))
nf <- layout(mat=matrix(1:4,1,4,byrow=T),widths=c(3,4,1,4),heights=3)
layout.show(nf)


par(mar=c(1,2,0,0),pty="s")
maxy <- max(c(foophist$counts,foohist$counts))
with(foo,hist(Distance.km,main="",
		las=1,xlim=c(0,450),breaks=seq(0,500,10),
		# col=adjustcolor("red",alpha.f=0.3),
		col="indianred1",
		freq=T,ylim=c(0,maxy),yaxt="n",xaxt="n",xlab="",ylab=""))
axis(side=2,at=seq(0,round(maxy/nrow(dat.passive),2),0.02)*nrow(dat.passive),labels=round(seq(0,round(maxy/nrow(dat.passive),2),0.02),2),las=1,cex.axis=cexl)

with(foop,hist(Distance.km,main="",
	las=1,xlim=c(0,450),breaks=seq(0,500,10),
	# col=adjustcolor("blue",alpha.f=0.3),
	col="dodgerblue",
	freq=T,add=T))

tmp <- foophist$counts
s1 <- smooth.spline(foophist$mids[1:max(which(tmp!=0))], foophist$counts[1:max(which(tmp!=0))],spar=0.5)
lines(s1,lwd=3,col="blue")
tmp <- foohist$counts
s1 <- smooth.spline(foohist$mids[1:max(which(tmp!=0))], foohist$counts[1:max(which(tmp!=0))], spar=0.5)
lines(s1,lwd=3,col="red")
legend('topright',legend=c("Swimming","Passive"),col=c("red","blue"),lwd=3,cex=1.2,bty="n")
mtext("a) 1D Dispersal",side=3,adj=0)
mtext("Distance from natal site (km)",side=1,line=3,cex=1.1)
mtext("Proportion settlement",side=2,line=2,at=0.53,outer=T,cex=1.1)
axis(side=1,at=seq(0,500,100),cex.axis=cexl)

par(mar=c(1,6,0,0))
cols <- plasma(10)[as.numeric(cut(M2[M2$proportion>0,which(names(M2)=="proportion")],breaks=10))]
plot(c(-300,300),c(-50,300),type="n",asp=1,las=1,xlab="",ylab="",cex.axis=cexl)
with(M2[M2$proportion>0,],points(xcoord,ycoord,cex=0.4,pch=19,col=cols))
abline(h=0,v=0,lty=2,col="grey")
draw.circle(0,0,radius=c(100,200,300,400,500),lty=2,border="grey")
mtext("b) Swimming",side=3,adj=0)
mtext("North-South distance\nfrom natal site (km)",side=2,line=3,outer=F,cex=1.1,at=-200,adj=0)

par(mar=c(20,3,20,0),pty="m")
color.bar(plasma(100),0,1)

par(mar=c(1,3.5,0,2.8),pty="s")
cols <- plasma(10)[as.numeric(cut(M2p[M2p$proportion>0,which(names(M2p)=="proportion")],breaks=10))]
plot(c(-300,300),c(-50,300),type="n",asp=1,las=1,xlab="",ylab="",cex.axis=cexl)
with(M2p[M2p$proportion>0,],points(xcoord,ycoord,cex=0.4,pch=19,col=cols))
abline(h=0,v=0,lty=2,col="grey")
draw.circle(0,0,radius=c(100,200,300,400,500),lty=2,border="grey")
mtext("c) Passive",side=3,adj=0)

mtext("East-West distance from natal site (km)",side=1,line=-18,at=0.65,outer=T,cex=1.1)
##########################################################


# dev.copy2pdf (file="Figure 3.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure 3.pdf",outfile='Figure 3.pdf') # journals often want fonts embedded

