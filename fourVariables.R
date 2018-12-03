# Packages
setwd("~/bayesNets")
library(bnlearn)
library(pcalg)
library(Rgraphviz)

# Functions
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

# Binary Discretization: 4 variables at a time

file.names <- dir('tempCodeOutput/outputFinal/')
for(i in 1:length(file.names)){
  # Load data for category
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  variables[9]<-file.names[i]
  colnames(data)[9]<-file.names[i]
  data[]<-lapply(data,as.factor)
  
  commonCauseScores<-c()
  commonEffectScores<-c()
  commonCausecommonEffect<-c()
  bestFitScores<-c()
  variableOrder<-c()  
  
  dir.create(paste('output/4Variables',file.names[i],sep = "/"))
  dir.create(paste('output/4Variables',file.names[i],'hc',sep = "/"))
  
  # iterate over combinations
  allCombinations<-combn(1:8,4,simplify = FALSE)
  for(j in 1:length(allCombinations)){
    # Extract data
    nodeList<-allCombinations[[j]]
    variableList<-c(variables[9],variables[nodeList[1]],variables[nodeList[2]],variables[nodeList[3]],variables[nodeList[4]])
    dataReduced<-data[,variableList]
    
    variableNames<-paste(variableList[2],variableList[3],variableList[4],variableList[5],sep = '+')
    variableOrder<-c(variableOrder,variableNames)
    
    # create graphs
    commonCause<-createCommonCauseGraph(variableList)
    ind<-createIndependentGraph(variableList)
    commonEffect<-createCommonEffectGraph(variableList)
    
    # Scores
    ccs<-BF(commonCause, ind, dataReduced, score = "bic")
    commonCauseScores<-c(commonCauseScores,ccs)
    cces<-BF( commonEffect,ind,dataReduced, score = "bic")
    commonEffectScores<-c(commonEffectScores,cces)
    ccces<-BF(commonCause, commonEffect, dataReduced, score = "bic")
    commonCausecommonEffect<-c(commonCausecommonEffect,ccces)
    
    
    # Generate Graph - hill climbing
    # print(i)
    path<-paste('output/4Variables',file.names[i],'hc',j,sep = "/")
    # print(path)
    png(filename=paste(path,'png',sep = '.'))
    plot(hc(dataReduced))
    dev.off()
    bestFitDag<-hc(dataReduced)
    
    bfs<-BF(bestFitDag, commonCause, dataReduced, score = "bic")
    bestFitScores<-c(bestFitScores,bfs)
  }
  df<-data.frame(variableOrder,commonCauseScores,commonEffectScores,
                 commonCausecommonEffect,bestFitScores)
  filePath<-paste('output/4Variables',file.names[i],'scores.csv',sep = "/")
  write.csv(df,file=filePath,row.names = FALSE)
  
}

