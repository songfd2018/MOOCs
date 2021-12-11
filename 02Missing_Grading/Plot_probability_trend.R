library(ggplot2)

N_vec <- c(1000,3000,10000)
m_vec <- c(3,4,5,6,7)
lam_vec <- c(0.60,0.70,0.80,0.90)
num_N <- length(N_vec)
num_m <- length(m_vec)
num_lam <- length(lam_vec)

prob_con <- array(NA, dim=c(num_N,num_m,num_lam))
time <- array(NA, dim=c(num_N,num_m,num_lam))

dimnames(prob_con) <- list(N_vec, m_vec, lam_vec)
dimnames(time) <- list(N_vec, m_vec, lam_vec)
for(index_N in 1:num_N){
  N <- N_vec[index_N]
  for(index_m in 1:num_m){
    m <- m_vec[index_m]
    for(index_lam in 1:num_lam){
      lam <- lam_vec[index_lam]
      
      file_name <- paste0("prob_id_v7_N",N,"_m",m,"_lambda",lam*100,".RData")
      message(paste0("Loading ",file_name,"\n"))
      if(file.exists(file_name)){
        load(file_name)
        prob_con[index_N,index_m,index_lam] <- prob_Y
        time[index_N,index_m,index_lam] <- time_consumption
      }else{
        message(paste0(file_name," doesn't exist.\n"))
      }
    }
  } 
}
save(prob_con,time,file="prob_trend_collection.RData")

# plot the probability given the number of class size when m = 3 ~ 7
# setwd("/home/sfd1994895/snap/MOOCs/code/0521code_for_submission/Missing_Grading/result")
load("prob_trend_collection.RData")

col_prob <- as.vector(prob_con)
col_time <- as.vector(time)

num_N <- dim(prob_con)[1]
num_m <- dim(prob_con)[2]
num_lam <- dim(prob_con)[3]

N_vec <- rownames(prob_con)
m_vec <- colnames(prob_con)
lam_vec <- colnames(prob_con[1,,])

col_N <- as.numeric(rep(N_vec,num_m * num_lam))
col_m <- as.numeric(rep(rep(m_vec, each = num_N),num_lam))
col_lam <- as.numeric(rep(lam_vec,each = num_N * num_m ))
col_group <- paste0("N",col_N,"_lam",col_lam * 100)
prob_frame <- data.frame(Prob = col_prob, Time = col_time,
                         N = factor(col_N), m = col_m, lambda = factor(col_lam), 
                         Group = factor(col_group))

jpeg(filename = "probability_trend.jpeg",width = 800, height = 600, quality = 100)
gsubset_compare <- ggplot(data=subset(prob_frame,lambda %in% c(0.5,0.6,0.7,0.8,0.9)), aes(x=m, y=Prob, group=Group, shape=N, colour=lambda, linetype =N)) +
  scale_colour_hue(h = c(250, 0)) + 
  geom_line(size=2.5) + geom_point(size=8) +
  geom_hline(yintercept=0.9, linetype="dashed",size=2.5) +
  labs(x = "m", y = "Pr(Realizing a valid Y)", colour = expression(lambda)) +
  theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black")) +
  theme(axis.text.x=element_text(size=30))+
  theme(axis.text.y=element_text(size=30))+
  theme(axis.title.x =element_text(hjust = 0.5,size=30))+
  theme(axis.title.y =element_text(hjust = 0.5,size=30)) +
  theme(legend.text=element_text(size=30), legend.title = element_text(size = 36, face ="bold"))
#guides(shape = guide_legend(reverse = TRUE))
gsubset_compare
dev.off()
