### This code produces Figure 5 in the manuscript: 
### Burgess SC, Bode M, Leis JM, Mason LB. Selection on dispersal traits and the emergence of dispersal kernels: how larval-fish swimming speed influences marine dispersal. Submitted for publication.

library(AICcmodavg)
library(extrafont)
loadfonts(device="postscript")

# setwd("")
load("d_Inshore_Swimming1000.RData")
load("d_Inshore_Passive1000.RData")

ind <- 1:length(d_Inshore_Swimming1000)

max.speed <- max(d_Inshore_Swimming1000[[1]][,3])

#### Calculate swimming speed vs settlement success
p <- vector(mode="list",length=length(ind))
p.excl <- vector(mode="list",length=length(ind))
p.passive <- NULL
p.excl.passive <- NULL
params <- data.frame(matrix(NA,nrow=length(ind),ncol=3))
names(params) <- c("K","Delta_AICc","exp_slope")

for(i in ind){
# Swimming data
	foo <- d_Inshore_Swimming1000[[i]]
	
	# Choose certain island groups
# foo <- foo[foo$Start.point.lat>-21.9 & foo$Start.point.lat < (-21.6) & foo$Start.point.long > 150 & foo$Start.point.long < 151 ,] # Percy 
# foo <- foo[foo$Start.point.lat>-23.23 & foo$Start.point.lat < (-23) & foo$Start.point.lon > 150.85 & foo$Start.point.lon < 151.03 ,] # Keppel 
# foo <- foo[foo$Start.point.lat>-23.94 & foo$Start.point.lat < (-23.1) & foo$Start.point.lon > 151.6 & foo$Start.point.lon < 152.45 ,] # Cap Bunk

	newdat <- with(foo, seq(min(Max.swimming.speed.ms),max(Max.swimming.speed.ms),length=100))

	m1 <- glm(Settlement ~ Max.swimming.speed.ms + I(Max.swimming.speed.ms^2), family="binomial", data=foo)
	m2 <- glm(Settlement ~ Max.swimming.speed.ms, family="binomial", data=foo)
	params[i,1] <- aictab(list(m1,m2))$K[2]
	params[i,2] <- aictab(list(m1,m2))$Delta_AICc[2]
	params[i,3] <- exp(coef(m1)[2]*0.1)
	ifelse(params[i,2]>4,m1<-m2,m1<-m1)

	p1 <- predict(m1,newdata=list(Max.swimming.speed.ms = newdat,Max.swimming.speed.ms2 = newdat^2),se.fit=T)
	d1 <- data.frame(	swimming.speed.ms = newdat,
						Prob = plogis(p1$fit)
					)
	p[[i]] <- d1

# Passive data
	foo <- d_Inshore_Passive1000[[i]]

	# Choose certain island groups
# foo <- foo[foo$Start.point.lat>-21.9 & foo$Start.point.lat < (-21.6) & foo$Start.point.long > 150 & foo$Start.point.long < 151 ,] # Percy 
# foo <- foo[foo$Start.point.lat>-23.23 & foo$Start.point.lat < (-23) & foo$Start.point.lon > 150.85 & foo$Start.point.lon < 151.03 ,] # Keppel 
# foo <- foo[foo$Start.point.lat>-23.94 & foo$Start.point.lat < (-23.1) & foo$Start.point.lon > 151.6 & foo$Start.point.lon < 152.45 ,] # Cap Bunk

	p.passive[i] <- round( (length(foo$Settlement[foo$Settlement=="1"]) / length(foo$Settlement) ) , 3)
}


# Make plot
quartz(width=4,height=4)
par(mfrow=c(1,1),mar=c(4,4,1,1),oma=c(0,0,1,0))
cex.tcks <- 1.2
cex.labs <- 1.2
lwds=2
plot(c(0,max.speed),c(0,1),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=seq(0,max.speed,0.02),cex=cex.tcks)
axis(side=2,at=seq(0,1,0.2),las=1,cex=cex.tcks)
mtext(expression(paste("Larval swimming speed (",ms^-1,")")),side=1,line=2.5,cex=cex.labs)
mtext("Probability of settlement",side=2,line=2.5,cex=cex.labs)


for(i in ind){
	foo <- p[[i]]
	with(foo,lines(swimming.speed.ms,Prob,lwd=lwds))
}
# points(rep(0,length(ind)),p.passive,pch=19,col=adjustcolor("black",alpha.f=0.4))
points(rep(0,length(ind)),p.passive,pch=19,col="grey40")
###############################################################

# dev.copy2pdf (file="Figure 5.pdf", family="ArialMT")
# Sys.setenv(R_GSCMD="/usr/local/bin/gs") # have to point R to the ghost script location.  
# # To find out where your ghost script is located, type system("which gs), then use that file path in code above.
# embed_fonts ("Figure 5.pdf",outfile='Figure 5.pdf') # journals often want fonts embedded


# Numbers reported in results section of paper
round(sort(as.numeric(lapply(p, function(x) x[1,2]))),2) # Slowest swimming
round(sort(as.numeric(lapply(p, function(x) x[dim(x)[1],2]))),2) # Fastest swimming
round(range(p[[12]][,2]),2) # Release 12
