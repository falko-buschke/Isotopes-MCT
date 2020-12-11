#################################################################################
#										#
#				Figure 1					#
#										#
#################################################################################

# Please note, since Figure 1 is only a conceptual diagram 
# of 2 hypotheitical species, it doesn't follow exactly the same
# approach as presented in the paper. This is because the hypothetical
# species do not have exact trait values.

# Start by assigning arbitrary body mass (BM) to species i and j.
BMi <- 50
BMj <- 200

# This the the maximum growth rate (R_max) for species i and j
# In the paper, we use Cole's equation based on life-history traits.
# Here, for illustrative purposes, we use approximate allometric scaling
# for species of specific masses. 
Ri <- (1.2826*BMi^-0.3)
Rj <- (1.2826*BMj^-0.3)

# This represents the maximum growth rates as lambda (finite rate of increase, L), 
# which is the exponent of the per capital growth rate
Li <- exp(Ri) ; Lj <- exp(Rj)

# Again, here we differ from the paper, which uses a slightly different scaling
# for daily resource requirements. The reason is to make the figure scaling more legible.
Fi <- BMi^0.8
Fj <- BMj^0.8

# This is parameter v, which is the rate at which growth rates increase with resource 
# availability (for species i and j). Here it is expressed in terms of the per-capital
# growth rates, r.
vir <- (Ri - log(exp(Ri)-1))/Fi
vjr <- (Rj - log(exp(Rj)-1))/Fj

# This is the same parameter, v, expressed in terms of the finite rate of incrase, lambda.
# See Appendix to paper.
vil <- log(Li/(Li-1))/Fi 
vjl <- log(Lj/(Lj-1))/Fj 

# This check just confirms that we get the same result, regardless of whether we use
# per-capital growth rate, or finite rate of increase.
vil == vir
vjl == vjr

# This is just a vector, which we will use for the x-axis.
R <- 0:200

# This is the per-capita growth rate as a function of resource availability
ri <- log(exp(Ri)-exp(Ri - vir*R))
rj <- log(exp(Rj)-exp(Rj - vjr*R))

# This is the finite rate of increase as a function of resource availability
li <- Li - (Li/(exp(vil*R)))
lj <- Lj - (Lj/(exp(vjl*R)))

#########################################################################
# Making the figure
png(filename="Fig1.png",width=24,height=8,units="cm",res=600)
par(mfrow=c(1,3)) 				# Three panels
par(mai=c(0.6,0.6,0.3,0.1))		# Set figure margins

# First panel is a hypothetical biplot of two species carbon- and notrogen-isotope niches
plot(0,0,type="n",ylim=c(-2,10), xlim=c(-30,-15),las= 1, mgp=c(2.8,0.8,0), 
	ylab=expression(paste(delta^{15}, "N (\u2030)")), 
	xlab = expression(paste(delta^{13}, "C (\u2030)")) , cex.axis=1.2, cex.lab= 1.4)

# Use "plotrix" package to draw circles. Install package if necessary
#install.packages("plotrix")
library(plotrix)
draw.circle(-20,3,4,col=rgb(1,0,0,0.4),border=F)
draw.circle(-24,6,3,col=rgb(0,0,0,0.3),border=F)

# Annotage the plot with labels
text (-24.5, 9.3, expression("Species"~ italic("i")),col="black",cex=1.5)
text (-19.5, -1.5, expression("Species"~ italic("j")),col="red",cex=1.5)
text (-22.35,4.75, expression(I*intersect(J)),cex=1.25)
text (-24.5, 6.5, "I|J",cex=1.25)
text (-19.5, 2.5, "J|I",cex=1.25)

# Label the panel
mtext("a",cex=1.3, side = 3, adj = -0.1, line = 0.5, font=2)

###########################################################
###########################################################

# Second panel is the relationship between per-capita growth rate and resource availability
plot(R,ri,col="black",ylim=c(-1.5,0.5),las= 1, type="l", mgp=c(2.8,0.8,0), xlab=expression("Resource level"~ italic("(R)")), 
	ylab = expression(italic("Per capita")~ "growth rate "~ italic("(r)")),lwd=1.5, cex.axis=1.2, cex.lab= 1.4)
lines(R,rj, col="red", lwd=1.5)
abline(h=0,col="grey")		# Equilbrium growth rate

# Add points for minimum resource requirments, with labels
text(Fi+5, 0, expression(italic(F[i])), pos=1,cex=1.2,col="black")
text(Fj+5, 0, expression(italic(F[j])), pos=1,cex=1.2,col="red")
points(Fi,0,pch=16,cex=1.6,col="black"); points (Fj,0,pch=16,cex=1.6,col="red")

# Add lines for maximum growth rate, with labels
abline(h=Ri,col="black", lty=3) ; abline(h=Rj,col="red",lty=3)
text(0,Ri+0.1,expression(italic(r[i]^0)),col="black",font=3)
text(0,Rj,pos=1,expression(italic(r[j]^0)),col="red", font=2)

# Calculate sensititivity to resource depletion, S, using equation 10
Sir <- vir/(exp(vir*Fi)-1)
Sjr <- vjr/(exp(vjr*Fj)-1)

# Add a line illustrating the sensitivity, S
ci <- -(Sir*Fi) 	# First calculate the intercept of the slope
abline(a=ci,b=Sir,col="black", lty=2, lwd=0.8)	# Add the line

cj <- -(Sjr*Fj)		# First calculate the intercept of the slope
abline(a=cj,b=Sjr,col="red", lty=2, lwd=0.8)	# Add the line


legend("bottomright", lty=1, col=c("black","red"), cex=1.2,
	c(expression("Species"~ italic("i")~"(small-bodied)"), expression("Species"~ italic("j")~"(large-bodied)")))

# Label the panel
mtext("b",cex=1.3, side = 3, adj = -0.1, line = 0.5, font=2)

###########################################################
###########################################################

# Third panel is the same as panel b, only that r is divided by S
plot(R,ri/Sir,col="black",las= 1, ylim=c(-150,50),type="l", mgp=c(2.8,0.8,0), xlab=expression("Resource level"~ italic("(R)")), 
	ylab = expression("Scaled growth rate "~ italic("(r/S)")),lwd=1.5, cex.axis=1.2, cex.lab= 1.4)
lines(R,rj/Sjr, col="red", lwd=1.5)
abline(h=0,col="grey")

# Add points for minimum resource requirments, with labels
text(Fi+5, 0, expression(italic(F[i])), pos=1,cex=1.2,col="black")
text(Fj+5, 0, expression(italic(F[j])), pos=1,cex=1.2,col="red")

points(Fi,0,pch=16,cex=1.6,col="black"); points (Fj,0,pch=16,cex=1.6,col="red")

# Add the parallel lines
abline(a=-Fi,b=1,col="black", lty=2, lwd=0.8)
abline(a=-Fj,b=1,col="red", lty=2, lwd=0.8)

# Add a legend
legend("bottomright", lty=1, col=c("black","red"), cex=1.2,
	c(expression("Species"~ italic("i")~"(small-bodied)"), expression("Species"~ italic("j")~"(large-bodied)")))

# Label the panel
mtext("c",cex=1.3, side = 3, adj = -0.1, line = 0.5, font=2)

dev.off()

##############################################################
##############################################################

#################################################################################
#										#
#				Figure S1				   	#
#										#
#################################################################################

# This is the same as in panle b of Figure 1, only for the finite rate of increase, lambda
png(filename="FigS1.png",width=12,height=12,units="cm",res=600)
# Set plot margins
par(mai=c(0.85,0.85,0.2,0.2))

# Make the plot of lambda as a function of resource availability
plot(R,li,col="black",las= 1, type="l", mgp=c(2.5,0.6,0), xlab=expression("Resource level"~ italic("(R)")), 
	ylab = expression("Finite rate of increase"~(lambda)),lwd=1.5)
lines(R,lj, col="red", lwd=1.5)
abline(h=1,col="grey")

# Add points for minimum resource requirments, with labels
text(Fi+5, 1, expression(italic(F[i])), pos=1,cex=1.2,col="black")
text(Fj+5, 1, expression(italic(F[j])), pos=1,cex=1.2,col="red")
points(Fi,1,pch=16,cex=1.2,col="black"); points (Fj,1,pch=16,cex=1.2,col="red")

# Add lines for maximum growth rate, with labels
abline(h=Li,col="black", lty=3) ; abline(h=Lj,col="red",lty=3)
text(0,Li,pos=1,expression(italic(lambda[i]^0)),col="black",font=3)
text(0,Lj,pos=1,expression(italic(lambda[j]^0)),col="red", font=2)

# Calculate sensititivity to resource depletion, S, using equation 10
Sil <- Li*vil/exp(vil*Fi)
Sjl <- Lj*vjl/exp(vjl*Fj)

# Add a line illustrating the sensitivity, S
ci <- 1 - (Sil*Fi)
abline(a=ci,b=Sil,col="black", lty=2, lwd=0.8)

cj <- 1 - (Sjl*Fj)
abline(a=cj,b=Sjl,col="red", lty=2, lwd=0.8)

# Add legend
legend("bottomright", lty=1, col=c("black","red"), 
	c(expression("Species"~ italic("i")~"(small-bodied)"), expression("Species"~ italic("j")~"(large-bodied)")))

dev.off()

