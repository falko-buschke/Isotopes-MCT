#################################################
#						#
# 		Figures 2 & 3			#
#						#
#################################################

# Import the isotop data from the included dataset (Isotope.data.txt)
KNP <-  read.table("Isotope_data.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# This is a vector of ID codes, which includes 12 months of data
# Naming convention = Northern Basalts (NB) + year + calendar month
sites <- c("Kruger_NB_2004_6",
	"Kruger_NB_2004_7",
	"Kruger_NB_2004_8",
	"Kruger_NB_2004_9",
	"Kruger_NB_2004_10",
	"Kruger_NB_2004_11",
	"Kruger_NB_2004_12",
	"Kruger_NB_2005_1",
	"Kruger_NB_2005_2",
	"Kruger_NB_2005_3",
	"Kruger_NB_2005_4",
	"Kruger_NB_2005_5")

# The labels that will be added to the plots for each site ID
labs <- c("Jun 2004","Jul 2004","Aug 2004","Sept 2004",
	"Oct 2004","Nov 2004","Dec 2004", "Jan 2005", "Feb 2005", "Mar 2005",
	"Apr 2005", "May 2005")

# The colour palette to be used for pairs of species
cols <- colorRampPalette(c(rgb(0,0,0.5,0.25),rgb(0,0.8,0.8,0.25),rgb(1,0.5,0,0.25)),interpolate="linear")(10)

# Names of species pairs. We use the common names, but scientific names are included in dataset
pairs <- c("Impala vs. Wildebeest",
	"Impala vs. Zebra",
	"Impala vs. Giraffe",
	"Impala vs. Buffalo",
	"Wildebeest vs. Zebra",
	"Wildebeest vs. Giraffe",
	"Wildebeest vs. Buffalo",
	"Zebra vs. Giraffe",
	"Zebra vs. Buffalo",
	"Giraffe vs. Buffalo")

# These are the polt symbols (cicle and diamonds) for alternating pairs of species
pch.val <- rep(c(16,18),5)

# This is the monthly rainfall in each month
rainfall <- c(12,0,0,0,11,36.5,46.7,58.3,0,59,1,1)
# Cumulative rainfall in preceding months (starts with 0)
cumRain <- c(0,cumsum(rainfall))


#################################################
#						#
# 		Figure 2			#
#						#
#################################################

png(filename="Fig2.png",width=24,height=20,units="cm",res=300)

#Set panel outline. The top panel is used for the legend
m <- (matrix(c(1,1,1,1,2,3,4,5,6,7,8,9,10,11,12,13), nrow = 4, ncol = 4, byrow = TRUE))

# Set heights for plot panles
layout(mat = m,heights = c(0.1,0.3,0.3,0.3))

# Set plot margins for first panel (legend)
par(mai=c(0,0,0.0,0.0))

# Make a blank plot and add a legend
plot(0,0,type="n",xlab="", ylab="", axes=F)
legend("center",pch=pch.val,cex=1.2,col=cols,pairs,ncol=5)

# Run a loop for the monthly biplots
# Set plot margins
par(mai=c(0.5,0.5,0.05,0.05))
for (k in 1:12) {
	# Obtain the subset of data for the specific month
	KNPsub <- KNP[which(KNP$community==sites[k]),]
 	# Identify the pair of species
 	pair <- paste(KNPsub$i," vs. ",KNPsub$j)

 	# Diatary data for I, J,and overlap (I intersect J)
	I <- KNPsub$SEAi_mean
	J <- KNPsub$SEAj_mean
	Ol <- KNPsub$Olap

	# Body masses of species i and j
	BMi <- KNPsub$BM_i
	BMj <- KNPsub$BM_j

	# Maximum growth rates for species i and j, from Cole's equation
	Ri <- KNPsub$Rmax_i
	Rj <- KNPsub$Rmax_j

	# Minimum resource requirements for species i and j
	Fi <- 0.05*BMi^0.77
	Fj <- 0.05*BMj^0.77

	# Rate of grrwoth rate increaase with additional resources (v), equation 9
	vi <- (Ri - log(exp(Ri)-1))/Fi 
	vj <- (Rj - log(exp(Rj)-1))/Fj

	# Sensitivity to resource depletion (S), equation 10
	Si <- vi/(exp(vi*Fi)-1)
	Sj <- vj/(exp(vj*Fj)-1)


	# Calcluate stabilisation (equation 7) and equalisation (equation 6)
	Stabilisation <- sqrt((Ol^2)/(I*J))
	Fitness <- log((Sj/Si)*sqrt(J/I))

	# Set the colour of the points
	colID <- KNPsub$parCol

	# Make a biplot of stabilisation and equalisation
	plot (0,0,type="n",xlab="Niche overlap",las=1, mgp=c(2.0,0.6,0),
		ylab="ln Fitness ratio",ylim=c(-5,5),xlim=c(0,1), cex.axis=1.1, cex.lab= 1.3)   

	# Add the grey polygon for the coexistence zone.
	p <- seq(0,1,l=1000); ll <- log(p); ul <- log(1/p)
	polygon(c(p,rev(p)),c(ul,rev(ll)), border=F,col="lightgrey")
	abline(h=0)

	# Add the pairs of species as points 
	points(Stabilisation,Fitness, pch=pch.val[colID], cex=2,col=cols[colID])

	# Add a lable for the panel to show the month and year
	text(0.6,4.5,paste(labs[k]),cex=1.2)

	# Add the rainfall in the month an the cumulative rainfall in preceding months
	text(0.85,-4,pos=3, paste(rainfall[k], "mm"), col="blue",cex=1.2)
	text(0.85,-4,pos=1, paste(cumRain[k], "mm"), col="darkblue",cex=1.2)


# This code simply adds the stabilisation and fitness differences in a new R-object
# It is used to plot Figure 3, but we creat it in the same loop used for figure 2
	if (k ==1) {
		FIT <- exp(Fitness)
		STAB <- Stabilisation
	} else {
		FIT <- cbind(FIT,exp(Fitness))
		STAB <- cbind(STAB,Stabilisation)
	}

}

dev.off()

################################################
################################################

#################################################
#						#
# 		Figure 3			#
#						#
#################################################

#Set up plot, panels and margins
png(filename="Fig3.png",width=28,height=14,units="cm",res=300)
par(mfrow=c(1,2))
par(mai=c(0.8,0.8,0.1,0.1))

# Creat a colour ramp for increasing cumulative rainfall
colss <- (colorRampPalette(c("lightblue","darkblue"),interpolate="linear")(250))

# Make a barplot for cumulative rainfall
mp <- barplot(cumRain[1:12],axes=F,ylim=c(0,250),col=colss[ceiling(cumRain[1:13])],
	 ylab="Cumulative rainfall (mm)" , xlab="Month",cex.axis=1, cex.lab= 1.3,mgp=c(2.6,0.6,0))

# Only label every second month, so that the axis is legible
axis(1,at=mp,labels=rep("",12))
axis(1,at=mp[c(1,3,5,7,9,11)],labels=labs[c(1,3,5,7,9,11)], cex.axis=0.84, padj=-.5)
axis(2,seq(0,250,by=50),seq(0,250,by=50),cex=1.1,las=1)

# Annotate the panel and add a bounding box
mtext("(a)",cex=1.8, side = 3, adj = 0.05, line = -2)
box()

##############################################

# Plot the average stabilisation and equalisation components from the loop used for Figure 2
# This is a line plot to link months in sequential order
plot(apply(STAB,2,mean),apply(FIT,2,function(x){exp(mean(log(x)))}), 
	ylab="Mean fitness ratio",xlab="Mean niche overlap",
	las=1,col="grey", type="l",ylim=c(0.1,0.37), xlim=c(0,0.31),
	, cex.axis=1.1, cex.lab= 1.3,mgp=c(2.6,0.6,0))

# Add the polygon for the coexistence zone
polygon(c(-0.1,1,-0.1),c(1,1,-0.1),col=rgb(0,0,0,0.075), border=F)

# Add the points for each month. I add them twice, once for the colour scale, and once for the boundary
points(apply(STAB,2,mean),apply(FIT,2,function(x){exp(mean(log(x)))}), 
	pch=16,col=colss[ceiling(cumRain[1:13])],cex=1.4)
points(apply(STAB,2,mean),apply(FIT,2,function(x){exp(mean(log(x)))}), 
	pch=1,cex=1.4)

# Annotate the points with labels
text(apply(STAB,2,mean),apply(FIT,2,function(x){exp(mean(log(x)))}), 
	pos=c(1,3,2,1,3,3,1,3,1,3,1,1),labs,cex=0.7)

# Label the plot
mtext("(b)",cex=1.8, side = 3, adj = 0.05, line = -2)
dev.off()
