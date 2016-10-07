#Lappas Algorithm for Team Assembly (Enhanced Steiner Tree)
#The algorithm is explained in "Finding a Team of Experts in Social Networks"
#we consider a graph which contains the scholars and skills and in this graph we find the steiner tree
#using a heuristic algorithm


Enhanced_Steiner<-function(required_skills, collaboration_network, noauth, g){
  
  
  if(length(required_skills)==1){
    
    sscholar<-which(collaboration_network[1:noauth,required_skills]==max(collaboration_network[1:noauth,required_skills]),arr.ind=TRUE)
    sscholar<-sscholar[1]
    temp_solution<-sscholar
    
  }
  
  else{
    
    temp_required_skills<-required_skills  
    no_skill<-length(required_skills)
      
    selected_scholars<-rep(0,0)
    scholars<-rep(0,0)
    
    #pickup a random skill
    random_skill<-sample(required_skills,1)
    required_skills<-required_skills [!required_skills %in% random_skill]
    
    for(i in 1:no_skill){
      
      avg_weight<-mean(collaboration_network[temp_required_skills[i],which(collaboration_network[temp_required_skills[i],1:noauth]>0)])
      selected_scholars<-append(selected_scholars,list(which(collaboration_network[temp_required_skills[i],1:noauth]>=avg_weight)))
      
      if(temp_required_skills[i]!=random_skill)
        scholars<-c(scholars,which(collaboration_network[temp_required_skills[i],1:noauth]>=avg_weight))
      
    } 
    
    #remove repetitive scholars
    scholars<-scholars[!duplicated(scholars)]
    
    #add the scholars to the temp solution
    temp_solution<-selected_scholars[[which(temp_required_skills==random_skill)]]
    temp_solution<-temp_solution[!duplicated(temp_solution)]
    
    counter<-0
    
    while(length(required_skills!=0) && length(scholars)>0){
      
      
      counter<-counter+1
      
      #find the next scholar that is the closest scholar to other scholars in the solution
      shortest_path_length<-shortest.paths(g, v=scholars, to=temp_solution)
      shortest_path_index<-which(shortest_path_length==min(shortest_path_length),arr.ind=TRUE)
      

      
      if(length(shortest_path_index)>0 && !is.infinite(shortest_path_length[shortest_path_index[1,1],shortest_path_index[1,2]])){
        
        flag<-FALSE
        
        for(i in 1:no_skill){
          
          if(scholars[shortest_path_index[1,1]]%in%selected_scholars[[i]]){
            required_skills<-required_skills [!required_skills %in% temp_required_skills[i]]
            flag=TRUE
          }
          
        }
        
        if(flag){
          if(counter==1){
            temp_solution<-c(scholars[shortest_path_index[1,1]],temp_solution[shortest_path_index[1,2]])
          }
          else{
            temp_solution<-c(temp_solution,scholars[shortest_path_index[1,1]])
          }
        }
        
        scholars<-scholars [!scholars %in% scholars[shortest_path_index[1,1]]]
        
        #remove all scholars who are not in the set of related concepts
        temp_scholars<-rep(0,0)
        k<-1
        while(k <= length(temp_required_skills)){
          if(temp_required_skills[k] %in% required_skills)
            temp_scholars<-c(temp_scholars,selected_scholars[[k]])
          k<-k+1
        }
        
        scholars<-scholars[scholars %in% temp_scholars]
        
        
      }
      
      else{
                
        sscholar<-which(collaboration_network[,required_skills]==max(collaboration_network[,required_skills]),arr.ind=TRUE)
        sscholar<-sscholar[1]
        scholars<-scholars [!scholars %in% sscholar]
        
        for(i in 1:no_skill){
          
          if(sscholar%in%selected_scholars[[i]]){
            required_skills<-required_skills [!required_skills %in% temp_required_skills[i]]
          }
          
        }
        
        
        temp_scholars<-rep(0,0)
        k<-1
        while(k <= length(temp_scholars)){
          if(temp_required_skills[k] %in% required_skills)
            temp_scholars<-c(temp_scholars,selected_scholars[[k]])
          k<-k+1
        }
        
        scholars<-scholars[scholars %in% temp_scholars]
        
      }
      
      temp_solution<-temp_solution[!duplicated(temp_solution)]
      
    }

  }
  
  return (list(solution=temp_solution))
  
}