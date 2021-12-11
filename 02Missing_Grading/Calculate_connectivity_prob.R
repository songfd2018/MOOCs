rm(list=ls())
library('getopt')

cal_prob_backward_cpp <- function(Clist, Elist, Flist, lambda){
  
  # Backward method 
  # where the connectivity probability given the first k markers is 
  # a linear combination of the connectivity probability given the frist k+1 markers
  N <- length(Clist)
  m <- length(Clist[[1]])
  
  # print the results after finish 5 % graders 
  if(N > 20 * m){
    print_per_iterations <- N %/% 20 
  }else{
    print_per_iterations <- N
  }
  
  
  # First, consider all possible circumstances of the last marker.
  # Optional submissions, which is actually D_(N - 1) cap E_{N-1} = D_{N-1} cap C_N
  CD_inter <- intersect(Clist[[N]],Flist[[N-1]])
  Mt <- length(CD_inter)
  
  # Compulsory submissions, which is actually E_(N - 1) \ D_{N-1}
  CD_diff <- setdiff(Clist[[N]],Flist[[N-1]])
  Ot <- length(CD_diff)
  
  args = list(CD_inter = as.integer(CD_inter), CD_diff = as.integer(CD_diff), 
              lambda = lambda[N], 
              Mt = as.integer(Mt), 
              Ot = as.integer(Ot))
  
  res_cpp <- .Call('Cal_prob_Nminus1', args)
  prob_Yt <- res_cpp
  
  if(N > 2 * m){
    for(t in (N-2):(N-m+1)){
      # Second, consider all possible circumstances of the t-th marker.
      
      cover_all_sub <- length(Flist[[t]]) == N
      
      # Optional old submissions, which is D_t cap E_t
      ED_inter <- intersect(Elist[[t]],Flist[[t]])
      Nt <- length(ED_inter)
      
      # Optional submissions, which is D_t cap C_t+1
      CD_inter <- intersect(Clist[[t+1]],Flist[[t]])
      Mt <- length(CD_inter)
      
      # Compulsory submissions, which is actually C_t+1 \ D_t
      CD_diff <- setdiff(Clist[[t+1]],Flist[[t]])
      Ot <- length(CD_diff)
      
      # Overall equivalent circumstance, D_t+1 cap E_t
      ED_inter2 <- intersect(Elist[[t]],Flist[[t+1]])
      
      # Optional new submissions, D_t+1 cap E_t+1
      ED_inter3 <- intersect(Elist[[t+1]],Flist[[t+1]])
      Ntp1 <- length(ED_inter3)
      
      args = list(ED_inter = as.integer(ED_inter), 
                  CD_inter = as.integer(CD_inter), 
                  CD_diff = as.integer(CD_diff), 
                  ED_inter2 = as.integer(ED_inter2),
                  ED_inter3 = as.integer(ED_inter3),
                  lambda = lambda[t+1], 
                  Nt = as.integer(Nt),
                  Mt = as.integer(Mt), 
                  Ot = as.integer(Ot),
                  Ntp1 = as.integer(Ntp1),
                  cover_all_sub = cover_all_sub,
                  prob_Ytp1 = prob_Yt)
      
      res_cpp <- .Call('Cal_prob_T', args)
      prob_Yt <- res_cpp
      # print(ED_inter)
      # print(prob_Y[[t]]$partition)
      # print(prob_Y[[t]]$prob)
      
      if(t%%print_per_iterations==0){
        message(paste0("Finish calculating the conditional valid probability given the first ",t," graders\n"))
      }
    }
    
    t <- m
    # Optional old submissions, which is D_t cap E_t
    ED_inter <- intersect(Elist[[t]],Flist[[t]])
    Nt <- length(ED_inter)
    
    # Optional submissions, which is D_t cap C_t+1
    CD_inter <- intersect(Clist[[t+1]],Flist[[t]])
    Mt <- length(CD_inter)
    
    # Compulsory submissions, which is actually C_t+1 \ D_t
    CD_diff <- setdiff(Clist[[t+1]],Flist[[t]])
    Ot <- length(CD_diff)
    
    # Overall equivalent circumstance, D_t+1 cap E_t
    ED_inter2 <- intersect(Elist[[t]],Flist[[t+1]])
    
    # Optional new submissions, D_t+1 cap E_t+1
    ED_inter3 <- intersect(Elist[[t+1]],Flist[[t+1]])
    Ntp1 <- length(ED_inter3)
    
    args_learn = list(ED_inter = as.integer(ED_inter), 
                      CD_inter = as.integer(CD_inter), 
                      CD_diff = as.integer(CD_diff), 
                      ED_inter2 = as.integer(ED_inter2),
                      ED_inter3 = as.integer(ED_inter3),
                      Nt = as.integer(Nt),
                      Mt = as.integer(Mt), 
                      Ot = as.integer(Ot),
                      Ntp1 = as.integer(Ntp1))
    res_learn <- .Call('Cal_prob_T_learn', args_learn)
    linear_comb <- res_learn[[1]]
    num_element <- res_learn[[2]]
    
    for(t in (N-m): m){
      # print(paste0("t=",t))
      args_linear = list(linear_comb = linear_comb,
                         num_element = num_element,
                         lambda = lambda[t],
                         prob_Ytp1 = prob_Yt)
      
      prob_Yt <- .Call('Cal_prob_T_linear', args_linear)
      
      if(t%%print_per_iterations==0){
        message(paste0("Finish calculating the conditional valid probability given the first ",t," graders\n"))
      }
      
    }
    
    for(t in (m-1):1){
      # Second, consider all possible circumstances of the t-th marker.
      
      cover_all_sub <- length(Flist[[t]]) == N
      
      # Optional old submissions, which is D_t cap E_t
      ED_inter <- intersect(Elist[[t]],Flist[[t]])
      Nt <- length(ED_inter)
      
      # Optional submissions, which is D_t cap C_t+1
      CD_inter <- intersect(Clist[[t+1]],Flist[[t]])
      Mt <- length(CD_inter)
      
      # Compulsory submissions, which is actually C_t+1 \ D_t
      CD_diff <- setdiff(Clist[[t+1]],Flist[[t]])
      Ot <- length(CD_diff)
      
      # Overall equivalent circumstance, D_t+1 cap E_t
      ED_inter2 <- intersect(Elist[[t]],Flist[[t+1]])
      
      # Optional new submissions, D_t+1 cap E_t+1
      ED_inter3 <- intersect(Elist[[t+1]],Flist[[t+1]])
      Ntp1 <- length(ED_inter3)
      
      args = list(ED_inter = as.integer(ED_inter), 
                  CD_inter = as.integer(CD_inter), 
                  CD_diff = as.integer(CD_diff), 
                  ED_inter2 = as.integer(ED_inter2),
                  ED_inter3 = as.integer(ED_inter3),
                  lambda = lambda[t+1], 
                  Nt = as.integer(Nt),
                  Mt = as.integer(Mt), 
                  Ot = as.integer(Ot),
                  Ntp1 = as.integer(Ntp1),
                  cover_all_sub = cover_all_sub,
                  prob_Ytp1 = prob_Yt)
      
      res_cpp <- .Call('Cal_prob_T', args)
      prob_Yt <- res_cpp
      # print(ED_inter)
      # print(prob_Y[[t]]$partition)
      # print(prob_Y[[t]]$prob)
      
      if(t%%print_per_iterations==0){
        message(paste0("Finish calculating the conditional valid probability given the first ",t," graders\n"))
      }
      
    }
  }else{
    for(t in (N-2):1){
      # Second, consider all possible circumstances of the t-th marker.
      
      cover_all_sub <- length(Flist[[t]]) == N
      
      # Optional old submissions, which is D_t cap E_t
      ED_inter <- intersect(Elist[[t]],Flist[[t]])
      Nt <- length(ED_inter)
      
      # Optional submissions, which is D_t cap C_t+1
      CD_inter <- intersect(Clist[[t+1]],Flist[[t]])
      Mt <- length(CD_inter)
      
      # Compulsory submissions, which is actually C_t+1 \ D_t
      CD_diff <- setdiff(Clist[[t+1]],Flist[[t]])
      Ot <- length(CD_diff)
      
      # Overall equivalent circumstance, D_t+1 cap E_t
      ED_inter2 <- intersect(Elist[[t]],Flist[[t+1]])
      
      # Optional new submissions, D_t+1 cap E_t+1
      ED_inter3 <- intersect(Elist[[t+1]],Flist[[t+1]])
      Ntp1 <- length(ED_inter3)
      
      args = list(ED_inter = as.integer(ED_inter), 
                  CD_inter = as.integer(CD_inter), 
                  CD_diff = as.integer(CD_diff), 
                  ED_inter2 = as.integer(ED_inter2),
                  ED_inter3 = as.integer(ED_inter3),
                  lambda = lambda[t+1], 
                  Nt = as.integer(Nt),
                  Mt = as.integer(Mt), 
                  Ot = as.integer(Ot),
                  Ntp1 = as.integer(Ntp1),
                  cover_all_sub = cover_all_sub,
                  prob_Ytp1 = prob_Yt)
      
      res_cpp <- .Call('Cal_prob_T', args)
      prob_Yt <- res_cpp
      # print(ED_inter)
      # print(prob_Y[[t]]$partition)
      # print(prob_Y[[t]]$prob)
      
      if(t%%print_per_iterations==0){
        message(paste0("Finish calculating the conditional probability given the first ",t," graders\n"))
      }
      
    }
  }
  

  
  # Compulsory submissions, which is actually C_t+1 \ D_t
  C1 <- Clist[[1]]
  Ot <- length(C1)
  
  # Optional new submissions, D_t+1 cap E_t+1
  ED_inter3 <- intersect(Elist[[1]],Flist[[1]])
  Ntp1 <- length(ED_inter3)
  
  args = list(C1 = as.integer(C1),
              ED_inter3 = as.integer(ED_inter3),
              lambda = lambda[N],
              Ot = as.integer(Ot),
              Ntp1 = as.integer(Ntp1),
              prob_Y1 = prob_Yt)
  
  res_cpp <- .Call('Cal_prob_0', args)
  prob_Y0 <- res_cpp
  # print(res_cpp)
  
  return(prob_Y0)
}

#example code: 
# Rscript Calculate_connectivity_prob.R -N 1000 -m 5 -t 0.8
spec = matrix(c(
  'ClassSize', 'N', 1, "integer",
  'AssignNum', 'm', 1, "integer",
  'GradeProb', 't', 2, "double",
  'GardeProbName', 'n', 2, "character"), byrow=TRUE, ncol=4)
opt = getopt(spec)
N<-opt$ClassSize#1000
m<-opt$AssignNum#5
if(!is.null(opt$GradeProb)){
  lambda<-opt$GradeProb#0.8
  lambda_list <- rep(lambda, N)
}else if(!is.null(opt$GardeProbName)){
  lambda_list<- unlist(read.table(paste0(opt$GardeProbName,".txt")))
}else{
  message("Please provide grader-spcific grading probability by -t or -n")
}



dyn.load("src/cal_prob.so")

# Generate the assessment grid
W <- matrix(0,N,N)
for(i in 1:N){
  marked_index <- (i+1:m -1)%%N + 1
  W[i,marked_index] <- 1
}


# auxiliary variable
# convert a matrix to two lists
C_list <- list()
for(i in 1:N){
  C_list[[i]] <- which(W[,i]>0)
}

# E_list the coverage ability of the last N-t markers
E_list <- list()
W_sum_back <- rep(0,N)
for(i in (N-1):1){
  W_sum_back <- W_sum_back + W[,i+1]
  E_list[[i]] <- which(W_sum_back>0)
}

# F_list the coverage ability of the first t markers
F_list <- list()
W_sum_front <- rep(0,N)
for(i in 1:N){
  W_sum_front <- W_sum_front + W[,i]
  F_list[[i]] <- which(W_sum_front>0)
}

# Backward method to calculate the probability iteratively
start <- Sys.time()
prob_Y <- cal_prob_backward_cpp(C_list , E_list, F_list, lambda_list)
end <- Sys.time()
  
time_consumption <- difftime(end, start, units = "secs")

if(!dir.exists("result")){
  dir.create("result")
}

if(!is.null(opt$GradeProb)){
  
  save(prob_Y,time_consumption,file = paste0("./result/prob_id_N",N,"_m",m,"_lambda",100*lambda,".RData"))
  message(paste0("The connectivity probability with N = ",N,", m = ",m," and lambda = ",lambda," is ", signif(prob_Y, 4),"\n"))
  message(paste0("The time consumption of our proposed algorithm is ", signif(time_consumption, 4)," secs.\n"))
  
}else if(!is.null(opt$GardeProbName)){
  
  save(prob_Y,time_consumption,file = paste0("./result/prob_id_N",N,"_m",m,"_",opt$GardeProbName,".RData"))
  message(paste0("The connectivity probability with N = ",N,", m = ",m," and lambda is loaded from ",opt$GardeProbName," is ", signif(prob_Y, 4),"\n"))
  message(paste0("The time consumption of our proposed algorithm is ", signif(time_consumption, 4)," secs.\n"))
  
}





