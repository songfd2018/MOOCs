library(Matrix)

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
N_set <- c(1000,3000,10000,30000)
m_set <- c(3,4,5,6,7)
num_N <- length(N_set)
num_m <- length(m_set)

# record the connectivity indicator
indicator_connect_BFS <- matrix(0, num_N, num_m)
colnames(indicator_connect_BFS) <- m_set
rownames(indicator_connect_BFS) <- N_set

indicator_connect_Det <- matrix(0, num_N, num_m)
colnames(indicator_connect_Det) <- m_set
rownames(indicator_connect_Det) <- N_set

# record the time consumption
time_consume_BFS <- matrix(0, num_N, num_m)
colnames(time_consume_BFS) <- m_set
rownames(time_consume_BFS) <- N_set

time_consume_Det <- matrix(0, num_N, num_m)
colnames(time_consume_Det) <- m_set
rownames(time_consume_Det) <- N_set


# pre-call the BFS_list function
N <- 6
m <- 3
wbyrow <- list()
wbyrow[N+1] <- NULL
wbycol <- list()
wbycol[N+1] <- NULL
for(i in 1:N){
  marked_index <- (i+1:m -1)%%N + 1
  marked_logical <- as.logical(marked_index)
  wbyrow[[i]] <-  sort(marked_index[marked_logical])
  for(j in marked_index[marked_logical]){
    wbycol[[j]] <- c(wbycol[[j]],i)
  }
}
visited <- BFS_list(wbyrow, wbycol,1)


for(n_index in 1:num_N){
  N <- N_set[n_index]
  for(m_index in 1:num_m){
    m <- m_set[m_index]
    
    # Simulate the assessment grid
    wbyrow <- list()
    wbyrow[N+1] <- NULL 
    wbycol <- list()
    wbycol[N+1] <- NULL
    for(i in 1:N){
      marked_index <- (i+1:m -1)%%N + 1
      marked_logical <- as.logical(marked_index)
      wbyrow[[i]] <-  sort(marked_index[marked_logical])
      for(j in marked_index[marked_logical]){
        wbycol[[j]] <- c(wbycol[[j]],i)
      }
    }
    
    
    # test identifiability by BFS-MOOCs algorithm
    start_time <- Sys.time()
    visited <- BFS_list(wbyrow, wbycol,1)
    if(sum(visited[[1]])==N){
      indicator_connect_BFS[n_index,m_index] <- 1
    }
    end_time <- Sys.time()
    time_consume_BFS[n_index,m_index] <- difftime(end_time,start_time, units="secs")
    message(paste0("The time consumption of BFS-MOOCs with N = ",N,", m = ",m," is ",time_consume_BFS[n_index,m_index]," secs.\n"))

    # test identifiability by calculating the determinant of X^TX
    start_time <- Sys.time()
    num_grading <- N * m

    X <- Matrix(0, nrow = num_grading, ncol = 2 * N - 1, sparse = TRUE)
    row_index <- 1
    for(i in 1:N){
      for(j in wbyrow[[i]]){
        X[row_index, i] <- 1
        if(j > 1){
          X[row_index, j - 1 + N] <- 1
        }
        row_index <- row_index + 1
      }
    }
    XTX <- t(X) %*% X
    if(!det(XTX)==0){
      indicator_connect_Det[n_index,m_index] <- 1
    }
    end_time <- Sys.time()
    time_consume_Det[n_index,m_index] <- difftime(end_time,start_time, units="secs")
    message(paste0("The time consumption of determinant-based method with N = ",N,", m = ",m," is ",time_consume_Det[n_index,m_index]," secs.\n"))
  }
}

save(time_consume_BFS,time_consume_Det,file = "id_check_consumption.RData")

# visualization 
library(ggplot2)
load("id_check_consumption.RData")
N_set <- as.numeric(rownames(time_consume_BFS))
m_set <- as.numeric(colnames(time_consume_BFS))
num_N <- length(N_set)
num_m <- length(m_set)

col_N <- rep(N_set,num_m *2)
col_m <- rep(rep(m_set, each = num_N),2)
col_time <- c(as.vector(time_consume_BFS),as.vector(time_consume_Det))
col_method <- rep(c("BFS", "Det"), each = num_N * num_m)
col_group <- paste0(col_method,"_m",col_m)

time_consumption <- data.frame(logN = log10(col_N), m = factor(col_m),
                               logTime = log10(col_time), Method = col_method, Group = factor(col_group))

g_compare <- ggplot(data=time_consumption, aes(x=logN, y=logTime, group=col_group, shape=Method, colour=m)) +
  scale_colour_hue(h = c(250, 0)) + 
  geom_line(size=2.5) + geom_point(size=8) +
  scale_x_continuous(
    breaks = log10(N_set),
    label = c("1,000", "3,000", "10,000","30,000")
  ) +
  scale_y_continuous(
    breaks = c(-2, -1, 0, 1, 2, 3),
    label = c("0.01", "0.1", "1","10","100","1000")
  ) +
  labs(x = "Class Size", y = "Time Consumption (Secs)") +
  theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black")) +
  theme(axis.text.x=element_text(size=30))+
  theme(axis.text.y=element_text(size=30))+
  theme(axis.title.x =element_text(hjust = 0.5,size=30))+
  theme(axis.title.y =element_text(hjust = 0.5,size=30)) +
  theme(legend.text=element_text(size=30), legend.title = element_text(size = 36, face ="bold")) +
  guides(shape = guide_legend(reverse = TRUE))
g_compare


