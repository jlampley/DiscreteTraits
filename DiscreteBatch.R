#You can use code you wrote for the correlation exercise here.
#tree <- read.tree("____PATH_TO_TREE_OR_SOME_OTHER_WAY_OF_GETTING_A_TREE____")
source("C:\\Users\\JAL\\Desktop\\PhyloMeth\\DiscreteTraits\\DiscreteFunctions.R")

phy <- get_study_tree("ot_485", "tree1")
q <- matrix(c(-0.3, 0.3, 0.5, -0.5), 2, 2, byrow=TRUE)
discretetrait1 <- sim.char (phy, q, 1, model="discrete")
discretetrait2 <- sim.char (phy, q, 1, model="discrete")

discrete.data <- cbind(discretetrait1, discretetrait2)
rownames(discrete.data) <- rownames(discretetrait1)

cleaned.discrete <- CleanData(phy, discrete.data)
tree <- cleaned.discrete$phy
moss <- cleaned.discrete$data
moss[moss==2]<-0

VisualizeData(tree, moss)

moss.char <- moss
moss.char[,1] <-as.character(moss[,1])
moss.char[,2] <- as.character(moss[,2])

#First, let's use parsimony to look at ancestral states
cleaned.discrete.phyDat <- phyDat(moss.char, type="USER", levels=c("0", "1")) #phyDat is a data format used by phangorn
anc.p <- ancestral.pars(tree, cleaned.discrete.phyDat)
plotAnc(tree, anc.p, 1)

#Do you see any uncertainty? What does that mean for parsimony?
#Yes, some. It means that for all the uncertain nodes, either state there 
#gives a tree with an equally good "state-topology" (with the least changes overall)

#now plot the likelihood reconstruction
anc.ml <- ancestral.pml(pml(tree, cleaned.discrete.phyDat), type="ml")
plotAnc(tree, anc.ml, 1, pos="topleft")

#How does this differ from parsimony? ------
#Likelihood doesn't have to treat all changes as equally likely; this method
#is based on a model of evolution (though this model can treat all changes 
#equally if you want.)  Likelihood methods search through many trees, picking
#the one with the highest probability given the data provided.
#This graph is all uncertainty at the nodes.  
#Why does it differ from parsimony?--------------------------
#Because the parsimony state tree is just trying to minimize the number of changes,
#primarily.  The likelihood tree uses prior assumptions about change possibility.
#This likelihood tree seems more likely because it is conservative:  it 
#doesn't over-estimate how the traits have changed over time.
#What does uncertainty mean?----------------------------
#The method cannot say with certainty that one state at a node gives you a more
#likely state tree than one with a different state at the same node.

#How many changes are there in your trait under parsimony? ------
#------[1] 71
parsimony.score <- parsimony(tree, cleaned.discrete.phyDat, method="fitch")
print(parsimony.score)

#Can you estimate the number of changes under a likelihood-based model? 

#Well, we could look at branches where the reconstructed state changed from 
#one end to the other. But that's not really a great approach: 
#at best, it will underestimate the number of changes 
#(we could have a change on a branch, then a change back, for example). 
#A better approach is to use stochastic character mapping.

estimated.histories <- make.simmap(tree, moss[,1], model="ARD", nsim=5)

#always look to see if it seems reasonable
plotSimmap(estimated.histories)

counts <- countSimmap(estimated.histories)
print(counts)

#Depending on your biological question, investigate additional approaches:
#  As in the correlation week, where hypotheses were examined by constraining rate matrices, one can constrain rates to examine hypotheses. corHMM, ape, and other packages have ways to address this.
#  Rates change over time, and this could be relevant to a biological question: have rates sped up post KT, for example. Look at the models in geiger for ways to do this.
#  You might observe rates for one trait but it could be affected by some other trait: you only evolve wings once on land, for example. corHMM can help investigate this.

fd_delta <- fitDiscrete(tree, moss.char, model="ARD", transform="delta")

fd_kappa <- fitDiscrete(tree, moss.char, model="ARD", transform="kappa")

plot(tree, show.tip.label = FALSE)
delta.tree.char.1 <- rescale(tree, model="delta", fd_delta$discretetrait1$opt$delta)

plot(rescale(tree, model="delta", fd_delta$discretetrait1$opt$delta), show.tip.label=FALSE)
plot(delta.tree.char.1)

