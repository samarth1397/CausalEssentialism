setwd("~/bayesNets")
library(bnlearn)
library(pcalg)
library(Rgraphviz)
library(gridExtra)

essentialGroup<-c('lion.csv','gorilla.csv','flamingo.csv','peacock.csv','goldfish.csv','ant.csv','snail.csv','iguana.csv','pig.csv')
nonessentialGroup<-c('balloon.csv','flute.csv','necklace.csv','microwave.csv' ,'taxi.csv','candle.csv','bucket.csv', 'umbrella.csv', 'desk.csv')

### Binary discretization; 4 parent restriction

file.names <- dir('tempCodeOutput/outputFinal/')
commonCauseScores<-c()
bestFitScores<-c()
indegree<-c()
indBestFitScores<-c()
outdegree<-c()
marginalLikelihood<-c()

indegreeNodesPerCategory<-c()
outdegreeNodesPerCategory<-c()
highestIndegreeValue<-c()
highestOutdegreeValue<-c()

m<-1
for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  
  dataReduced<-data
  dataReduced$label<-NULL
  
  variables<-colnames(data)
  variables[9]<-file.names[i]
  colnames(data)[9]<-file.names[i]
  
  data[]<-lapply(data,as.factor)
  dataReduced[]<-lapply(dataReduced,as.factor)
  
  # Common cause graph
  commonCause<-empty.graph(nodes=variables)
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[1] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[2] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[3] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[4] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[5] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[6] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[7] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[8] )
  
  # Independent Graph
  ind<-empty.graph(nodes=variables)

  # Generate scores
  print('common cause/ind')
  ccs<-BF(commonCause, ind, data, score = "bic")
  print(ccs)
  commonCauseScores<-c(commonCauseScores,ccs)
  
  # Generate Graphs - hill climbing
  x<-paste('output/hc',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  bestFitDag<-hc(data,maxp = 4)
  plot(bestFitDag)
  dev.off()
  
  # bayes factor of best fit score to common cause
  bfs<-BF(bestFitDag, commonCause, data, score = "bic")
  bestFitScores<-c(bestFitScores,bfs) 
  
  # Append in degree and out-degree
  ind<-in.degree(bestFitDag,variables[9])
  outd<-out.degree(bestFitDag,variables[9])
  indegree<-c(indegree,ind)
  outdegree<-c(outdegree,outd)

  # Indegree of all nodes
  indegreeOfAllNodes<-c()
  for(i in 1:length(variables)){
    indegreeOfAllNodes<-c(indegreeOfAllNodes,in.degree(bestFitDag,variables[i]))
  }
  maxIndegIndices<-which(indegreeOfAllNodes==max(indegreeOfAllNodes))
  maxIndeg<-indegreeOfAllNodes[maxIndegIndices[1]]
  highestIndegreeValue<-c(highestIndegreeValue,maxIndeg)
  
  maxIndegNodes<-c()
  for(i in 1:length(maxIndegIndices)){
    maxIndegNodes<-paste(maxIndegNodes,variables[maxIndegIndices[i]],sep = '#')
    
  } 
  
  indegreeNodesPerCategory<-c(indegreeNodesPerCategory,maxIndegNodes)
  
  # Outdegree of all nodes
  outdegreeOfAllNodes<-c()
  for(i in 1:length(variables)){
    outdegreeOfAllNodes<-c(outdegreeOfAllNodes,out.degree(bestFitDag,variables[i]))
  }
  maxOutdegIndices<-which(outdegreeOfAllNodes==max(outdegreeOfAllNodes))
  maxOutdeg<-outdegreeOfAllNodes[maxOutdegIndices[1]]
  highestOutdegreeValue<-c(highestOutdegreeValue,maxOutdeg)
  
  maxOutdegNodes<-c()
  for(i in 1:length(maxOutdegIndices)){
    maxOutdegNodes<-paste(maxOutdegNodes,variables[maxOutdegIndices[i]],sep = '#')
    
  } 
  
  outdegreeNodesPerCategory<-c(outdegreeNodesPerCategory,maxOutdegNodes)
  
  
  # Best fit dag without label node
  bestFitIndDag<-hc(dataReduced,maxp = 4)
  
  adj<-amat(bestFitIndDag)
  y<-rep(0, 9)
  adj<-cbind(adj,y)
  adj<-rbind(adj,y)
  
  colnames(adj)[9]<-file.names[m]
  rownames(adj)[9]<-file.names[m]
  
  indBestFit<-empty.graph(variables)
  amat(indBestFit)<-adj

  x<-paste('output/hc-ind',file.names[m],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(indBestFit)
  dev.off()
  
  bfinds<-BF(commonCause, indBestFit,data, score = "bic")
  indBestFitScores<-c(indBestFitScores,bfinds)
  
  m<-m+1
  
  marginalScore<-logLik(commonCause,data,nodes=variables[-9])/logLik(bestFitIndDag,dataReduced)
  marginalLikelihood<-c(marginalLikelihood,marginalScore)
  
    
}

df<-data.frame(commonCauseScores,bestFitScores,indBestFitScores,indegree,outdegree,marginalLikelihood)
rownames(df)<-file.names
write.csv(df,file='output/scores.csv')

x<-t.test(df[essentialGroup,"commonCauseScores"],df[nonessentialGroup,"commonCauseScores"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"outdegree"],df[nonessentialGroup,"outdegree"])
x

df<-data.frame(indegreeNodesPerCategory,highestIndegreeValue,outdegreeNodesPerCategory,highestOutdegreeValue)
rownames(df)<-file.names
write.csv(df,file='output/scores-Degrees.csv')


grid.table(df)


### 3-Bin discretization; 2 parent restriction

file.names <- dir('tempCodeOutput/outputFinal2/')
commonCauseScores<-c()
bestFitScores<-c()
indBestFitScores<-c()
marginalLikelihood<-c()

indegree<-c()
outdegree<-c()
indegreeNodesPerCategory<-c()
outdegreeNodesPerCategory<-c()
highestIndegreeValue<-c()
highestOutdegreeValue<-c()

m<-1
for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal2',file.names[i],sep = "/"),header=TRUE,sep=',')
  
  dataReduced<-data
  dataReduced$label<-NULL
  
  variables<-colnames(data)
  variables[9]<-file.names[i]
  colnames(data)[9]<-file.names[i]
  
  data[]<-lapply(data,as.factor)
  dataReduced[]<-lapply(dataReduced,as.factor)
  
  # Common cause graph
  commonCause<-empty.graph(nodes=variables)
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[1] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[2] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[3] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[4] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[5] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[6] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[7] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[8] )
  
  # Independent Graph
  ind<-empty.graph(nodes=variables)
  
  # Generate scores
  print('common cause/ind')
  ccs<-BF(commonCause, ind, data, score = "bic")
  print(ccs)
  commonCauseScores<-c(commonCauseScores,ccs)
  
  # Generate Graphs - hill climbing
  x<-paste('output/hc-3',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  bestFitDag<-hc(data,maxp = 3)
  plot(bestFitDag)
  dev.off()
  
  # bayes factor of best fit score to common cause
  bfs<-BF(bestFitDag, commonCause, data, score = "bic")
  bestFitScores<-c(bestFitScores,bfs) 
  
  # Indegree/outdegree  
  ind<-in.degree(bestFitDag,variables[9])
  outd<-out.degree(bestFitDag,variables[9])
  indegree<-c(indegree,ind)
  outdegree<-c(outdegree,outd)
  
  # Indegree of all nodes
  indegreeOfAllNodes<-c()
  for(i in 1:length(variables)){
    indegreeOfAllNodes<-c(indegreeOfAllNodes,in.degree(bestFitDag,variables[i]))
  }
  maxIndegIndices<-which(indegreeOfAllNodes==max(indegreeOfAllNodes))
  maxIndeg<-indegreeOfAllNodes[maxIndegIndices[1]]
  highestIndegreeValue<-c(highestIndegreeValue,maxIndeg)
  
  maxIndegNodes<-c()
  for(i in 1:length(maxIndegIndices)){
    maxIndegNodes<-paste(maxIndegNodes,variables[maxIndegIndices[i]],sep = '#')
    
  } 
  
  indegreeNodesPerCategory<-c(indegreeNodesPerCategory,maxIndegNodes)
  
  # Outdegree of all nodes
  outdegreeOfAllNodes<-c()
  for(i in 1:length(variables)){
    outdegreeOfAllNodes<-c(outdegreeOfAllNodes,out.degree(bestFitDag,variables[i]))
  }
  maxOutdegIndices<-which(outdegreeOfAllNodes==max(outdegreeOfAllNodes))
  maxOutdeg<-outdegreeOfAllNodes[maxOutdegIndices[1]]
  highestOutdegreeValue<-c(highestOutdegreeValue,maxOutdeg)
  
  maxOutdegNodes<-c()
  for(i in 1:length(maxOutdegIndices)){
    maxOutdegNodes<-paste(maxOutdegNodes,variables[maxOutdegIndices[i]],sep = '#')
    
  } 
  
  outdegreeNodesPerCategory<-c(outdegreeNodesPerCategory,maxOutdegNodes)
  
  # Best fit dag without label node
  bestFitIndDag<-hc(dataReduced,maxp = 4)
  
  adj<-amat(bestFitIndDag)
  y<-rep(0, 9)
  adj<-cbind(adj,y)
  adj<-rbind(adj,y)
  
  colnames(adj)[9]<-file.names[m]
  rownames(adj)[9]<-file.names[m]
  
  indBestFit<-empty.graph(variables)
  amat(indBestFit)<-adj
  
  x<-paste('output/hc-ind',file.names[m],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(indBestFit)
  dev.off()
  
  bfinds<-BF(commonCause, indBestFit,data, score = "bic")
  indBestFitScores<-c(indBestFitScores,bfinds)
  
  m<-m+1
  
  marginalScore<-logLik(commonCause,data,nodes=variables[-9])/logLik(bestFitIndDag,dataReduced)
  marginalLikelihood<-c(marginalLikelihood,marginalScore)
}

df<-data.frame(commonCauseScores,bestFitScores,indBestFitScores,indegree,outdegree,marginalLikelihood)
rownames(df)<-file.names
write.csv(df,file='output/scores3Bins.csv')

x<-t.test(df[essentialGroup,"commonCauseScores"],df[nonessentialGroup,"commonCauseScores"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"outdegree"],df[nonessentialGroup,"outdegree"])
x

df<-data.frame(indegreeNodesPerCategory,highestIndegreeValue,outdegreeNodesPerCategory,highestOutdegreeValue)
rownames(df)<-file.names
write.csv(df,file='output/scores3Bins-Degrees.csv')


grid.table(df)


### Binary discretization; Reduced Set

file.names <- dir('tempCodeOutput/outputFinalReduced/')
commonCauseScores<-c()
bestFitScores<-c()
indBestFitScores<-c()
marginalLikelihood<-c()

indegree<-c()
outdegree<-c()
indegreeNodesPerCategory<-c()
outdegreeNodesPerCategory<-c()
highestIndegreeValue<-c()
highestOutdegreeValue<-c()

m<-1
for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinalReduced',file.names[i],sep = "/"),header=TRUE,sep=',')
  dataReduced<-data
  dataReduced$label<-NULL
  
  variables<-colnames(data)
  variables[9]<-file.names[i]
  colnames(data)[9]<-file.names[i]
  
  data[]<-lapply(data,as.factor)
  dataReduced[]<-lapply(dataReduced,as.factor)
  
  
  # Common cause graph
  commonCause<-empty.graph(nodes=variables)
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[1] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[2] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[3] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[4] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[5] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[6] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[7] )
  commonCause<-set.arc(commonCause,from = variables[9], to = variables[8] )
  
  # Independent Graph
  ind<-empty.graph(nodes=variables)
  
  # Generate scores
  print('common cause/ind')
  ccs<-BF(commonCause, ind, data, score = "bic")
  print(ccs)
  commonCauseScores<-c(commonCauseScores,ccs)
  
  # Generate Graphs - hill climbing
  x<-paste('output/hc-Reduced',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  bestFitDag<-hc(data,maxp = 3)
  plot(bestFitDag)
  dev.off()
  
  # bayes factor of best fit score to common cause
  bfs<-BF(bestFitDag, commonCause, data, score = "bic")
  bestFitScores<-c(bestFitScores,bfs) 
  
  
  # Indegree/outdegree  
  ind<-in.degree(bestFitDag,variables[9])
  outd<-out.degree(bestFitDag,variables[9])
  indegree<-c(indegree,ind)
  outdegree<-c(outdegree,outd)
  
  # Indegree of all nodes
  indegreeOfAllNodes<-c()
  for(i in 1:length(variables)){
    indegreeOfAllNodes<-c(indegreeOfAllNodes,in.degree(bestFitDag,variables[i]))
  }
  maxIndegIndices<-which(indegreeOfAllNodes==max(indegreeOfAllNodes))
  maxIndeg<-indegreeOfAllNodes[maxIndegIndices[1]]
  highestIndegreeValue<-c(highestIndegreeValue,maxIndeg)
  
  maxIndegNodes<-c()
  for(i in 1:length(maxIndegIndices)){
    maxIndegNodes<-paste(maxIndegNodes,variables[maxIndegIndices[i]],sep = '#')
    
  } 
  
  indegreeNodesPerCategory<-c(indegreeNodesPerCategory,maxIndegNodes)
  
  # Outdegree of all nodes
  outdegreeOfAllNodes<-c()
  for(i in 1:length(variables)){
    outdegreeOfAllNodes<-c(outdegreeOfAllNodes,out.degree(bestFitDag,variables[i]))
  }
  maxOutdegIndices<-which(outdegreeOfAllNodes==max(outdegreeOfAllNodes))
  maxOutdeg<-outdegreeOfAllNodes[maxOutdegIndices[1]]
  highestOutdegreeValue<-c(highestOutdegreeValue,maxOutdeg)
  
  maxOutdegNodes<-c()
  for(i in 1:length(maxOutdegIndices)){
    maxOutdegNodes<-paste(maxOutdegNodes,variables[maxOutdegIndices[i]],sep = '#')
    
  } 
  
  outdegreeNodesPerCategory<-c(outdegreeNodesPerCategory,maxOutdegNodes)
  
  # Best fit dag without label node
  bestFitIndDag<-hc(dataReduced,maxp = 4)
  
  adj<-amat(bestFitIndDag)
  y<-rep(0, 9)
  adj<-cbind(adj,y)
  adj<-rbind(adj,y)
  
  colnames(adj)[9]<-file.names[m]
  rownames(adj)[9]<-file.names[m]
  
  indBestFit<-empty.graph(variables)
  amat(indBestFit)<-adj
  
  x<-paste('output/hc-ind',file.names[m],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(indBestFit)
  dev.off()
  
  bfinds<-BF(commonCause, indBestFit,data, score = "bic")
  indBestFitScores<-c(indBestFitScores,bfinds)
  
  m<-m+1
  marginalScore<-logLik(commonCause,data,nodes=variables[-9])/logLik(bestFitIndDag,dataReduced)
  marginalLikelihood<-c(marginalLikelihood,marginalScore)
}


df<-data.frame(commonCauseScores,bestFitScores,indBestFitScores,indegree,outdegree,marginalLikelihood)
rownames(df)<-file.names
write.csv(df,file='output/scoresReduced.csv')

x<-t.test(df[essentialGroup,"commonCauseScores"],df[nonessentialGroup,"commonCauseScores"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"bestFitScores"],df[nonessentialGroup,"bestFitScores"])
x
x<-t.test(df[essentialGroup,"outdegree"],df[nonessentialGroup,"outdegree"])
x

df<-data.frame(indegreeNodesPerCategory,highestIndegreeValue,outdegreeNodesPerCategory,highestOutdegreeValue)
rownames(df)<-file.names
write.csv(df,file='output/scoresReduced-Degrees.csv')

grid.table(df)

