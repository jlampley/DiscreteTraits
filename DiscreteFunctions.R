library(ape)
library(geiger) 
library(corHMM)
library(phytools)
library(phangorn)
library(phylolm)
library(rotl)

#You can use code you wrote for the correlation exercise here.
VisualizeData <- function(phy, data) {
  windows()
  plot (phy)
  print(str(phy))
  print(dim(data))
  windows()
  barplot(data)
  print(table(data))
  windows()
  hist(data[,1])
  windows()
  hist(data[,2])
  plot(phy) 

#VisualizeData <- function(phy, data) 
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
}

#CleanData <- function(phy, data) 
	#treedata() in Geiger is probably my favorite function in R.
  CleanData <- function(phy, data) {
    
    Nelumbotree <- treedata(phy, data)
    return (Nelumbotree)
  } 
