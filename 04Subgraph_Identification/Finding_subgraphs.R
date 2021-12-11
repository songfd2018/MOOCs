library(scales) # for alph
library(RColorBrewer) # for Set3

# We use the method similar to BFS to detect the connectivity
BFS_list<-function(Wbyrow, Wbycol, init_vertex){
  
  visited_sub <- rep(0,N) # 0 for unvisted vertex, 1 for visited vertex
  
  visited_gra <- rep(0,N) # 0 for unvisted vertex, 1 for visited vertex
  
  queue <- init_vertex # 0 for new vertex, 1 for visited vertex
  visited_sub[init_vertex] <- 1
  # regard the first row as the 
  
  while(length(queue)>0){
    s<-queue[1]
    #print(s)
    queue <- queue[-1]
    
    # the grader indices of whom is not visited
    sub_s <- Wbyrow[[s]] 
    s_row <- sub_s[visited_gra[sub_s]==0]

    for(i in s_row){
      visited_gra[i] <- 1
      
      graded_i <- Wbycol[[i]] 
      adj_vertex <- graded_i[visited_sub[graded_i]==0]
      
      queue <- c(queue,adj_vertex)
      visited_sub[adj_vertex] <- 1
    }
  }
  
  return(list(marked_student=visited_sub,visited_markers=visited_gra))
}

# the set of N and m values
N <- 300
m <- 4

# Simulate an observed assessment grid
lambda <- 0.7
set.seed(1234)
ybyrow <- list()
ybyrow[N+1] <- NULL 
ybycol <- list()
ybycol[N+1] <- NULL
for(i in 1:N){
  marked_index <- (i+1:m -1)%%N + 1
  obs_grading <- rbinom(m,size = 1, prob = lambda)
  ybyrow[[i]] <- sort(marked_index[obs_grading == 1])
  for(j in marked_index[obs_grading == 1]){
    ybycol[[j]] <- c(ybycol[[j]],i)
  }
}

# identify all subgraphs
ungraded <- 1:N
subgraph_ind <- rep(0,N)
while(length(ungraded)!=0){
  subgraph_ind[ungraded] <- subgraph_ind[ungraded] + 1
  visited <- BFS_list(ybyrow, ybycol,ungraded[1])
  visited_sub <- which(visited$marked_student==1)
  ungraded <- setdiff(ungraded, visited_sub)
}

# visualization 
# Converting the data to an igraph object
library("igraph")
sub_obs <- data.frame(id = paste0("sub",1:N), num_graded = sapply(ybyrow, length))
gra_obs <- data.frame(from = NULL, to = NULL, weight = NULL)

for(i in 1:N){ # for each grader, add a complete subgraph into the submission graph
  if(length(ybycol[[i]])>1){
    edges_complete <- combn(ybycol[[i]],2)
    gra_obs <- rbind(gra_obs, cbind(t(matrix(paste0("sub",edges_complete),nrow = 2)),1))
  }
}
colnames(gra_obs) <- c("from","to","weight")
gra_obs$weight <- as.numeric(gra_obs$weight)

# insert an instructor into observed assessment grid
grid_obs <- graph_from_data_frame(d=gra_obs, vertices=sub_obs, directed=F) 
grid_obs <- simplify(grid_obs, edge.attr.comb="sum")

# node color by the index of subgraphs
# node size by the degree
ver_colors <- alpha(rainbow(max(subgraph_ind)), 0.6)
V(grid_obs)$color <- ver_colors[subgraph_ind]
V(grid_obs)$size <- (sub_obs$num_graded + 1) * 3

set.seed(12345)
l <- layout_with_fr(grid_obs)
E(grid_obs)$width <- E(grid_obs)$weight * 2
# E(grid_assign)$width <- E(grid_assign)$weight
jpeg(file="visual_subgraphs.jpeg",width = 800, height = 800, quality = 100)
plot(grid_obs, vertex.label=NA, layout = l)
legend(x=-1.3, y=-0.7, paste0("subgraph ",1:max(subgraph_ind)), pch=21,
       col="#777777", pt.bg=ver_colors, pt.cex=3, bty="n", ncol=1, cex = 2)
dev.off()
