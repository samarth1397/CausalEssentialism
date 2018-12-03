setwd("~/bayesNets")
library(bnlearn)
library(pcalg)
library(Rgraphviz)

# Alpha=0.3

file.names <- dir('tempCodeOutput/outputFinal/')
latentCounts<-c()

for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  c<-cov(data,data)
  cor<-cov2cor(c)
  cor<-matrix(cor,nrow = 8,ncol = 8)
  
  indTest<-binCItest
  suffStat<-list(dm=data,adaptDF=FALSE)
  
  # FCI Algorithm
  dag<-fci(suffStat = suffStat,indTest,alpha=0.3,labels = variables[-9],m.max = 4,pdsep.max = 4)
  x<-paste('output/fci0.3',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(dag)
  dev.off()
  adjMatrix<-wgtMatrix(dag)

  count=0
  for(i in 1:8){
    for(j in 1:8){
      if(adjMatrix[i,j]==2 & adjMatrix[j,i]==2){
        count=count+1
      }
    }
  }
  print(count/2)
  latentCounts<-c(latentCounts,count/2)
  
}

df<-data.frame(file.names,latentCounts)
write.csv(df,file='output/latent0.3.csv',row.names = FALSE)


# Alpha is 0.2

file.names <- dir('tempCodeOutput/outputFinal/')
latentCounts<-c()

for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  c<-cov(data,data)
  cor<-cov2cor(c)
  cor<-matrix(cor,nrow = 8,ncol = 8)
  
  indTest<-binCItest
  suffStat<-list(dm=data,adaptDF=FALSE)
  
  # FCI Algorithm
  dag<-fci(suffStat = suffStat,indTest,alpha=0.2,labels = variables[-9],m.max = 4,pdsep.max = 4)
  x<-paste('output/fci0.2',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(dag)
  dev.off()
  adjMatrix<-wgtMatrix(dag)
  
  count=0
  for(i in 1:8){
    for(j in 1:8){
      if(adjMatrix[i,j]==2 & adjMatrix[j,i]==2){
        count=count+1
      }
    }
  }
  print(count/2)
  latentCounts<-c(latentCounts,count/2)
  
}

df<-data.frame(file.names,latentCounts)
write.csv(df,file='output/latent0.2.csv',row.names = FALSE)


# Alpha is 0.7

file.names <- dir('tempCodeOutput/outputFinal/')
latentCounts<-c()

for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  c<-cov(data,data)
  cor<-cov2cor(c)
  cor<-matrix(cor,nrow = 8,ncol = 8)
  
  indTest<-binCItest
  suffStat<-list(dm=data,adaptDF=FALSE)
  
  # FCI Algorithm
  dag<-fci(suffStat = suffStat,indTest,alpha=0.7,labels = variables[-9],m.max = 4,pdsep.max = 4)
  x<-paste('output/fci0.7',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(dag)
  dev.off()
  adjMatrix<-wgtMatrix(dag)
  
  count=0
  for(i in 1:8){
    for(j in 1:8){
      if(adjMatrix[i,j]==2 & adjMatrix[j,i]==2){
        count=count+1
      }
    }
  }
  print(count/2)
  latentCounts<-c(latentCounts,count/2)
  
}

df<-data.frame(file.names,latentCounts)
write.csv(df,file='output/latent0.7.csv',row.names = FALSE)


# Alpha is 0.5

file.names <- dir('tempCodeOutput/outputFinal/')
latentCounts<-c()

for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  c<-cov(data,data)
  cor<-cov2cor(c)
  cor<-matrix(cor,nrow = 8,ncol = 8)
  
  indTest<-binCItest
  suffStat<-list(dm=data,adaptDF=FALSE)
  
  # FCI Algorithm
  dag<-fci(suffStat = suffStat,indTest,alpha=0.5,labels = variables[-9],m.max = 4,pdsep.max = 4)
  x<-paste('output/fci0.5',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(dag)
  dev.off()
  adjMatrix<-wgtMatrix(dag)
  
  count=0
  for(i in 1:8){
    for(j in 1:8){
      if(adjMatrix[i,j]==2 & adjMatrix[j,i]==2){
        count=count+1
      }
    }
  }
  print(count/2)
  latentCounts<-c(latentCounts,count/2)
  
}

df<-data.frame(file.names,latentCounts)
write.csv(df,file='output/latent0.5.csv',row.names = FALSE)

# Alpha is 0.25

file.names <- dir('tempCodeOutput/outputFinal/')
latentCounts<-c()

for(i in 1:length(file.names)){
  # Load data
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  c<-cov(data,data)
  cor<-cov2cor(c)
  cor<-matrix(cor,nrow = 8,ncol = 8)
  
  indTest<-binCItest
  suffStat<-list(dm=data,adaptDF=FALSE)
  
  # FCI Algorithm
  dag<-fci(suffStat = suffStat,indTest,alpha=0.25,labels = variables[-9],m.max = 4,pdsep.max = 4)
  x<-paste('output/fci0.25',file.names[i],sep = "/")
  png(filename=paste(x,'png',sep = '.'))
  plot(dag)
  dev.off()
  adjMatrix<-wgtMatrix(dag)
  
  count=0
  for(i in 1:8){
    for(j in 1:8){
      if(adjMatrix[i,j]==2 & adjMatrix[j,i]==2){
        count=count+1
      }
    }
  }
  print(count/2)
  latentCounts<-c(latentCounts,count/2)
  
}

df<-data.frame(file.names,latentCounts)
write.csv(df,file='output/latent0.25.csv',row.names = FALSE)


### Multiple alphas

file.names <- dir('tempCodeOutput/outputFinal/')
latentCounts<-c()
ccFeatures<-c()

x<-seq(0.2,0.725,0.025)
for(i in 1:length(x)){
  path1<-paste('output/hidden/fci',x[i],sep='')
  dir.create(path1)
}

for(i in 1:length(file.names)){
  # Load data
  hiddencounts<-c()
  print(file.names[i])
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  c<-cov(data,data)
  cor<-cov2cor(c)
  cor<-matrix(cor,nrow = 8,ncol = 8)
  
  indTest<-binCItest
  suffStat<-list(dm=data,adaptDF=FALSE)
  
  potentialCommonCause<-c()
  scores<-matrix(0,nrow = 8,ncol = 8)
  colnames(scores)<-variables[-9]
  rownames(scores)<-variables[-9]
    
  for(i in 1:length(x)){
    dag<-fci(suffStat = suffStat,indTest,alpha=x[i],labels = variables[-9],m.max = 4,pdsep.max = 4)
    adjMatrix<-wgtMatrix(dag)
    
    count=0
    for(i in 1:8){
      for(j in 1:8){
        if(adjMatrix[i,j]==2 & adjMatrix[j,i]==2){
          count=count+1
          scores[i,j]=scores[i,j]+1
        }
      }
    }

    path1<-paste('output/hidden/fci',x[i],sep='')
    path2<-paste(path1,file.names[i],sep = "/")
    png(filename=paste(path2,'png',sep = '.'))
    plot(dag)
    dev.off()
    
    hiddencounts<-c(hiddencounts,count/2)
  }
  scores[lower.tri(scores)]<-0
  for(i in 1:8){
    for(j in 1:8){
      if(scores[i,j]>2){
        pair<-paste(variables[i],variables[j],scores[i,j],sep = '+')
        potentialCommonCause<-paste(potentialCommonCause,pair,sep='##')
      }
    }
  }
  
  latentCounts<-rbind(latentCounts,hiddencounts)
  if(is.null(potentialCommonCause)){
    potentialCommonCause<-c('noFeatures')
  }
  print(potentialCommonCause)
  ccFeatures<-rbind(ccFeatures,potentialCommonCause)
}

latentCounts<-cbind(latentCounts,ccFeatures)
rownames(latentCounts)<-file.names
colnames(latentCounts)<-c(x,'features')
write.csv(latentCounts,file='output/latent.csv')


### Binary discretization; 4 parent restriction

file.names <- dir('tempCodeOutput/outputFinal/')
for(i in 1:length(file.names)){
  # Load data
  data<-read.csv(paste('tempCodeOutput/outputFinal',file.names[i],sep = "/"),header=TRUE,sep=',')
  variables<-colnames(data)
  data$label<-NULL
  
  variableOrder<-c()  
  dir.create(paste('output/3VariablesHidden',file.names[i],sep = "/"))
  dir.create(paste('output/3VariablesHidden',file.names[i],'fci',sep = "/"))
  
  latentCounts<-c()
  
  # Iterate over combinations
  allCombinations<-combn(1:8,4,simplify = FALSE)
  for(j in 1:length(allCombinations)){
    # Extract data
    nodeList<-allCombinations[[j]]
    variableList<-c(variables[nodeList[1]],variables[nodeList[2]],variables[nodeList[3]],variables[nodeList[4]])
    dataReduced<-data[,variableList]
    
    variableNames<-paste(variableList[1],variableList[2],variableList[3],variables[nodeList[4]],sep = '+')
    variableOrder<-c(variableOrder,variableNames)
    covariance<-cov(dataReduced,dataReduced)
    correlation<-cov2cor(covariance)
    correlation<-matrix(correlation,nrow = 4,ncol = 4)
    # print(correlation)
    indTest<-binCItest
    suffStat<-list(dm=data,adaptDF=FALSE)
    
    dag<-fci(suffStat = suffStat,indTest,alpha=0.05,labels = variableList)
    
    path<-paste('output/3VariablesHidden',file.names[i],'fci',j,sep = "/")
    png(filename=paste(path,'png',sep = '.'))
    plot(dag)
    dev.off()
    
    adjMatrix<-wgtMatrix(dag)
    
    count=0
    for(m in 1:4){
      for(n in 1:4){
        if(adjMatrix[m,n]==2 & adjMatrix[n,m]==2){
          count=count+1
        }
      }
    }
    print(count/2)
    latentCounts<-c(latentCounts,count/2)
    
  }
  
  df<-data.frame(variableOrder,latentCounts)
  filePath<-paste('output/3VariablesHidden',file.names[i],'hiddenCounts.csv',sep = "/")
  write.csv(df,file=filePath,row.names = FALSE)
    
}





