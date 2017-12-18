#################EEB & Flow 2017 caRd
#First choose how to get the data:

#IF you chose to download the data file already, make sure you have the local file pathway correctly here:
myfile <- "2017caRd_data.csv"
newImage <- read.csv(myfile, header=TRUE)


#OR, use RCurl package to download it directly from github 
require(RCurl)
myfile <- getURL("https://raw.githubusercontent.com/cmtucker/EEB-FlowMisc/master/2017caRd_data.csv", ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
newImage <- read.csv(textConnection(myfile), header=TRUE)


####################Run the rest of the script
#This library is necessary, you may need to install 
#install.packages("deldir")
require(deldir) 

###Start data processing
rw = c(1, 500, 1, 500)

#number of colours to cluster to
kColors <- 150

#Kmean cluster points
kMeans <- kmeans(newImage[, 3:5], centers = kColors)
kMeansColor <- rgb(kMeans$centers[kMeans$cluster, ])

#calculate tesselations (deldir library)
newtess <- deldir(newImage, rw=rw, sort=TRUE, dpl=list(ndx=NULL))
w <- tile.list(newtess)

###Start card plotting
#Make device with set size, works on multiple OS
dev.new <- function(width = 5, height = 5){
	platform <- sessionInfo()$platform 
if (grepl("linux",platform)) { 
	x11(width=width, height=height) 
	}else{ 
if (grepl("pc",platform)) { 
	windows(width=width, height=height) 
	}else{
if (grepl("w32",platform)) { 
	windows(width=width, height=height) 
	}else{		
if (grepl("apple", platform)){ 
	quartz(width=width, height=height)} }}}}

#Plotting starts
dev.new(6, 6)
par(bg="grey10", mai=c(0.1,0.1,0.1,0.1), oma=c(0.1,0.1,0.1,0.1))

#Empty
plot(x=newImage$x, y=newImage$y, col = rgb(newImage[3:5]), asp = 1, pch=".", xlim=c(rw[1:2]), ylim=rw[3:4], xaxt="n", yaxt="n", ylab="", xlab="", bty="n", type="n")

#View tesselations
plot(newtess, wlines="tess", wpoints="none", number=FALSE, lty=1, showrect=FALSE,  xlim=c(min(cImage$x), max(cImage$x)), ylim=c(min(cImage$y), max(cImage$y)), bty="n", xaxt="n", yaxt="n", ylab="", xlab="", bty="n", add=TRUE)

#And fill
plot(w, fillcol= kMeansColor, close=FALSE, showpoints=FALSE,  xlim=c(rw[1:2]), ylim=rw[3:4], add=TRUE)
text(x=375, y=475, "Happy Holidays from \n the EEB & Flow", col="white", cex=1.75, font=1)

#Fin