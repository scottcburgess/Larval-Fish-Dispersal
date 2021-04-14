### This code produces Figure S1 in the manuscript: 
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

max.distance <- 350
# Get distribution of predicted dispersal distances per degree of latitude
coords <- seq(from=-500, to=500, by=10) #coords
cexl <- 1.2


quartz(width=8,height=7)
par(mfrow=c(5,5),mar=c(1,2,0,0),oma=c(4,3,1,1))


for(r in ind){
	### Swimming model
	# m is a matrix with xcoords as columns and ycoords as rows
	m<-matrix(NA, ncol=length(coords), nrow=length(coords))

	colnames(m)<-coords #xcoords
	row.names(m)<-coords #ycoords

	 # For loop to fill in the matrix 
	# j is the data frame with the model predictions
	dat <- do.call(rbind.data.frame,d_Inshore_Swimming1000)
	j <- dat[dat$Settlement==1 & dat$Release.event==r,]

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
	j <- dat.passive[dat.passive$Settlement==1 & dat.passive$Release.event==r,]

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


plot(c(-max.distance,max.distance),c(-max.distance,max.distance),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="l")

cols <- viridis(10)[as.numeric(cut(M2[M2$proportion>0,which(names(M2)=="proportion")],breaks=10))]
with(M2[M2$proportion>0,],points(xcoord,ycoord,cex=0.4,pch=19,col="red"))

cols <- viridis(10)[as.numeric(cut(M2p[M2p$proportion>0,which(names(M2p)=="proportion")],breaks=10))]
with(M2p[M2p$proportion>0,],points(xcoord,ycoord,cex=0.4,pch=19,col="blue"))

abline(h=0,v=0,lty=2,col="grey")
draw.circle(0,0,radius=c(100,200,300,400,500),lty=2,border="grey")

points(0,0,pch=8,cex=1.5,col="black")
legend('bottomleft',legend=eval(paste("Release",r)),bty="n",cex=1.2)

if(r==20|r==21|r==22|r==23|r==24)
	axis(side=1,at=seq(-max.distance,max.distance,100))
if(r==1|r==6|r==11|r==16|r==21)
	axis(side=2,at=seq(-max.distance,max.distance,100),las=1)
}

plot(c(0,1),c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n",bty="n")
legend('center',legend=c("Swimming","Passive","Origin"),col=c("red","blue","black"),pch=c(19,19,8),cex=1.3,bty="n")
mtext("East-West Distance (km) from natal site",side=1,line=2,outer=T,cex=1.2)
mtext("North-South Distance (km) from natal site",side=2,line=1.5,outer=T,cex=1.2)
##########################################################

# dev.copy2pdf (file="Figure S1.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure S1.pdf",outfile='Figure S1.pdf') # journals often want fonts embedded

