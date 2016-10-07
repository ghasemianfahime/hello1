#This script is for the implementation of my algorithm for Team Assembly based on the rank of scholars in a hypergraph

library('igraph')
library('intergraph')
library('Matrix')

MyAlgorithm<-function(required_skills, adjacency_matrix1, adjacency_matrix2, noScholar, Ts, neighbor_radius){
  
  concept_sim<-Matrix(0,noScholar,length(required_skills))
  alpha<-0.9
  
  for(i in 1:length(required_skills)){
    
    score<-rep(0,nrow(adjacency_matrix1))
    score[required_skills[i]]<-1
    query<-score
    
    for(k in 1:20){
      score<-(alpha*adjacency_matrix1%*%score)+((1-alpha)*query)
    }
    
    score<-score[1:noScholar]    
    if(max(score)!=0)
      score<-as.vector(score/max(score))
    
    concept_sim[,i]<-score
    
  }
  
  score<-rep(0,nrow(adjacency_matrix1))
  score[required_skills]<-1
  
  query<-score
  alpha<-0.9
  
  for(k in 1:1){
    score<-(alpha*adjacency_matrix1%*%score)+((1-alpha)*query)
  }
  
  score<-score[1:noScholar]
  query<-score
  
  for(k in 2:20){
    score<-(alpha*adjacency_matrix2%*%score)+((1-alpha)*query)
  }
  
  score<-as.vector(score/max(score))
  
  #determine the potential regions
  Candidate_Regions_Centers<-rep(0,0)
  
  aa<-sort(score,decreasing=TRUE,index.return = TRUE)
  
  score_value<-aa$x
  score_index<-aa$ix
  #   print(score_index)
  
  Candidate_Regions_Centers<-score_index[which(score_value>Ts)]
  
  Temp_Candidate_Regions_Centers<-Candidate_Regions_Centers
  
  diag(adjacency_matrix2)<-0
  g<-graph.adjacency(adjacency_matrix2,weighted=TRUE,"undirected")
  result_jaccard<-similarity.jaccard(g,Candidate_Regions_Centers)
  diag(result_jaccard)<-0
  
  # print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
  
  if(length(Candidate_Regions_Centers)>0){
    
    for(i in 1:nrow(result_jaccard)){
      
      if(length(which(Temp_Candidate_Regions_Centers==Candidate_Regions_Centers[i]))>0){
        
        for(j in 1:nrow(result_jaccard)){
          
          if(length(which(Temp_Candidate_Regions_Centers==Candidate_Regions_Centers[j]))>0){
            
            if(result_jaccard[i,j]>0.5 || adjacency_matrix2[Candidate_Regions_Centers[i],Candidate_Regions_Centers[j]]>0){
              Temp_Candidate_Regions_Centers<-Temp_Candidate_Regions_Centers[-which(Temp_Candidate_Regions_Centers==Candidate_Regions_Centers[j])]
            }
            
          }
          
        }
        
      }
      
    }
    
    
    
    solutions<-Matrix(0,length(Temp_Candidate_Regions_Centers),length(required_skills))
    fitness<-rep(0,length(Temp_Candidate_Regions_Centers))
    
    #execute Simulated Annealing for each region separately
    for(i in 1:length(Temp_Candidate_Regions_Centers)){
      
      print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      print(Temp_Candidate_Regions_Centers[i])
      
      solutions[i,]<-Find_Best_Solution_Region(Temp_Candidate_Regions_Centers[i], required_skills, adjacency_matrix2, noScholar, Ts, neighbor_radius,concept_sim)
      fitness[i]<-calcFitness(solutions[i,],required_skills, Ts,concept_sim)
      
    }
    
    Sfitness<-sort(fitness,decreasing=TRUE,index.return=TRUE)
    final_solution<-solutions[Sfitness$ix[1],]
    
    #Integrate Solutions
    final_solution<-Integrate_Solutions(alpha, final_solution, required_skills, adjacency_matrix2, noScholar, Ts, concept_sim)
    
    return (final_solution)
    
  }
  
  else{
    
    return(-1)
    
  }
  
}


Find_Best_Solution_Region<-function(starting_point, required_skills, adjacency_matrix2, noScholar,Ts, neighbor_radius, concept_sim){
  
  solution<-rep(0,length(required_skills))
  search_space<-rep(0,0)
  
  neighbors<-starting_point
  
  for(i in 1:neighbor_radius){
    
    if(length(neighbors)>1){
      neighbors<-unique(which(adjacency_matrix2[neighbors,]>0,arr.ind=TRUE)[,2])
    }
    else{
      neighbors<-unique(which(adjacency_matrix2[neighbors,]>0))
    }
    
    search_space<-c(search_space,neighbors)  
    
  }
  
  search_space<-unique(search_space)
  
  for(i in 1:length(required_skills)){
    
    max_index<-which.max(concept_sim[search_space,i])
    if(length(max_index)>0)
      solution[i]<-search_space[max_index]
    else
      solution[i]<--1
    
  }
  
  return (solution)
  
}


calcFitness<-function(solution,required_skills, Ts, concept_sim){
  
  fitness <- 0
  
  for(i in 1:length(required_skills)){
    
    if(solution[i]>0)
      fitness <- fitness + concept_sim[solution[i],i]
    
  }
  
  return (fitness)
  
}


Integrate_Solutions<-function(alpha, solution, required_skills, adjacency_matrix2, noScholar,Ts, concept_sim){
  
  selected_solution_scholars<-solution[which(solution>Ts)]
  score<-rep(0,nrow(adjacency_matrix2))
  
  score[selected_solution_scholars]<-1
  query<-score
  
  for(k in 1:20){
    score<-(alpha*adjacency_matrix2%*%score)+((1-alpha)*query)
  }
  
  score<-as.vector(score/max(score))
  
  
  for(i in 1:length(required_skills)){
    
    if(solution[i]!=-1 && concept_sim[solution[i],i]<Ts){
      
      #Find another scholar from other regions to replace it with the current scholar
      candidate_scholars<-which(concept_sim[,i]>Ts)
      
      if(length(candidate_scholars>0)){
        
        candidate_scores<-score[candidate_scholars]
        bb=which.max(candidate_scores)
        solution[i]<-candidate_scholars[bb]
        
      }
      
    }
    
  }
  
  return (solution)
  
}