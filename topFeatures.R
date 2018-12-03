# Packages
setwd("~/bayesNets")
library(bnlearn)
library(pcalg)
library(Rgraphviz)

createCommonCauseGraph<-function(variableList){
  commonCause<-empty.graph(nodes=variableList)
  commonCause<-set.arc(commonCause,from = variableList[1], to = variableList[2] )
  commonCause<-set.arc(commonCause,from = variableList[1], to = variableList[3] )
  commonCause<-set.arc(commonCause,from = variableList[1], to = variableList[4] )
  commonCause<-set.arc(commonCause,from = variableList[1], to = variableList[5] )
  return(commonCause)
}

createIndependentGraph<-function(variableList){
  ind<-empty.graph(nodes=variableList)
  return(ind)
}

createCommonEffectGraph<-function(variableList){
  commonEffect<-empty.graph(nodes=variableList)
  commonEffect<-set.arc(commonEffect,from = variableList[2], to = variableList[1] )
  commonEffect<-set.arc(commonEffect,from = variableList[3], to = variableList[1] )
  commonEffect<-set.arc(commonEffect,from = variableList[4], to = variableList[1] )
  commonEffect<-set.arc(commonEffect,from = variableList[5], to = variableList[1] )
}


essentialGroup<-c('lion.csv','gorilla.csv','flamingo.csv','peacock.csv','goldfish.csv','ant.csv','snail.csv','iguana.csv','pig.csv')
nonessentialGroup<-c('balloon.csv','flute.csv','necklace.csv','microwave.csv' ,'taxi.csv','candle.csv','bucket.csv', 'umbrella.csv', 'desk.csv')


c1<-c('can.roar','has.a.mane', 'has.a.tail','has.fur')
c2<-c('likes.to.eats.bananas','can.swing.from.trees','is.black','is.dangerous')
c3<-c('can.fly', 'can.stand.on.one.leg','has.a.beak','has.a.long.neck')
c4<-c('has.feathers', 'is.colourful','has.long.tail.feathers', 'is.blue')
c5<-c( 'can.swim', 'has.fins','has.scales','is.orange')
c6<-c('has.legs','can.bite','can.crawl','is.black')
c7<-c('has.a.shell','is.slow','is.slimy','is.found.in.gardens')
c8<-c('is.green', 'is.scaly','has.a.tail','has.a.tongue')
c9<-c('has.a.snout','is.pink','has.a.curly.tail','likes.mud')
c10<-c('can.float','made.of.rubber','is.colourful','requires.helium')
c11<-c('made.of.metal','is.long','has.holes','used.by.blowing.air.through')
c12<-c('is.worn.around.the.neck','made.of.gold','made.of.pearls','made.of.silver')
c13<-c('is.found.in.kitchens','can.cook.food','can.heat','made.of.metal')
c14<-c('is.yellow','is.black','made.of.metal','has.a.meter')
c15<-c( 'has.a.wick','made.of.wax','provides.light','produces.heat')
c16<-c('has.a.handle.handles','made.of.metal','made.of.plastic','can.contain.liquid')
c17<-c('protects.from.the.rain','has.a.handle.handles','is.watertight.waterproof','is.collapsible')
c18<-c('made.of.wood','has.legs','has.drawers','is.flat')

foo<-vector(mode="list",length=18)
names(foo)<-c('lion.csv','gorilla.csv','flamingo.csv','peacock.csv','goldfish.csv','ant.csv','snail.csv','iguana.csv','pig.csv',
              'balloon.csv','flute.csv','necklace.csv','microwave.csv' ,'taxi.csv','candle.csv','bucket.csv', 'umbrella.csv', 'desk.csv') 
foo[[1]]<-c1
foo[[2]]<-c2
foo[[3]]<-c3
foo[[4]]<-c4
foo[[5]]<-c5
foo[[6]]<-c6
foo[[7]]<-c7
foo[[8]]<-c8
foo[[9]]<-c9
foo[[10]]<-c10
foo[[11]]<-c11
foo[[12]]<-c12
foo[[13]]<-c13
foo[[14]]<-c14
foo[[15]]<-c15
foo[[16]]<-c16
foo[[17]]<-c17
foo[[18]]<-c18

file.names <- dir('tempCodeOutput/outputFinal/')
commonCauseScores<-c()
commonEffectScores<-c()
commonCausecommonEffect<-c()
bestFitScores<-c()
indegree<-c()
outdegree<-c()

for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  variables[9]<-file.names[i]
  colnames(data)[9]<-file.names[i]
  data[]<-lapply(data,as.factor)
  variableList<-c(variables[9],foo[[file.names[i]]])
  dataReduced<-data[,variableList]
  
  commonCause<-createCommonCauseGraph(variableList)
  ind<-createIndependentGraph(variableList)
  commonEffect<-createCommonEffectGraph(variableList)
  
  ccs<-BF(commonCause, ind, dataReduced, score = "bic")
  commonCauseScores<-c(commonCauseScores,ccs)
  cces<-BF( commonEffect,ind,dataReduced, score = "bic")
  commonEffectScores<-c(commonEffectScores,cces)
  ccces<-BF(commonCause, commonEffect, dataReduced, score = "bic")
  commonCausecommonEffect<-c(commonCausecommonEffect,ccces)
  
    # Generate Graphs - hill climbing
  x<-paste('output/topFeaturesHC',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  bestFitDag<-hc(dataReduced,maxp = 4)
  plot(bestFitDag)
  dev.off()
  
  # bayes factor of best fit score to common cause
  bfs<-BF(bestFitDag, commonCause, dataReduced, score = "bic")
  bestFitScores<-c(bestFitScores,bfs) 
  
  # Append in degree and out-degree
  ind<-in.degree(bestFitDag,variables[9])
  outd<-out.degree(bestFitDag,variables[9])
  indegree<-c(indegree,ind)
  outdegree<-c(outdegree,outd)

}

df<-data.frame(commonCauseScores,commonEffectScores,commonCausecommonEffect,bestFitScores,indegree,outdegree)
rownames(df)<-file.names
write.csv(df,file='output/topFeatures.csv')

x<-t.test(df[essentialGroup,"commonCauseScores"],df[nonessentialGroup,"commonCauseScores"])
x
x<-t.test(df[essentialGroup,"commonEffectScores"],df[nonessentialGroup,"commonEffectScores"])
x

x<-t.test(df[essentialGroup,"commonCausecommonEffect"],df[nonessentialGroup,"commonCausecommonEffect"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"outdegree"],df[nonessentialGroup,"outdegree"])
x














