# Compare different distribution of good and bad students
lambda_good <- 0.9
lambda_bad <- 0.7

N <- 1000

# number of consecutive good or bad students
consec_list <- c(1,2,4,5,10,20,50,100,250,500)
N_con <- length(consec_list)

# Generate the grading probablity list for each circumstance
for(i in 1:N_con){
  consec <- consec_list[i]
  lambda_list2 <- rep(rep(c(lambda_good,lambda_bad),each = consec), N/consec/2)
  write.table(lambda_list2,file = paste0("prob_N",N,"_consec",consec,".txt"),
              row.names = F, col.names = F)
}

if(!dir.exists("result")){
  dir.create("result")
}

# Run "Calculate_connectivity_prob.R" for each setting of m and grader-specific grading probability lambda
# this step can be run in complete parallel
m_sel <- c(4,5,6)
for(m in m_sel){
  for(consec in consec_list){
    system(paste0("Rscript Calculate_connectivity_prob.R -N ",N," -m ",m," -n prob_N",N,"_consec",consec))
  }
}

# Collect the overall result and plot scatter plot
num_m <- length(m_sel)
prob_con <- matrix(NA, num_m, N_con)
time <- matrix(NA, num_m, N_con)

rownames(prob_con) <- m_sel
colnames(prob_con) <- consec_list
rownames(time) <- m_sel
colnames(time) <- consec_list

for(i in 1:num_m){
  for(j in 1:N_con){
    m <- m_sel[i]
    consec <- consec_list[j]
    result_name <- paste0("./result/prob_id_N",N,"_m",m,"_prob_N",N,"_consec",consec,".RData")
    if(file.exists(result_name)){
      load(result_name)
      prob_con[i,j] <- prob_Y
      time[i,j] <- time_consumption
    }else{
      message(paste0(result_name," doesn't exist.\n"))
    }
  }
}

save(prob_con,time,file="prob_trend_permutation_graders.RData")

# plot the probability under different permutation of good and bad students when m = 4 ~ 6
load("prob_trend_permutation_graders.RData")
library(reshape2)
library(ggplot2)
prob_frame <- melt(prob_con)
colnames(prob_frame) <- c("m","consec","Prob")

jpeg(filename = "probability_trend_different_permutation.jpeg",width = 800, height = 600, quality = 100)
gsubset_compare <- ggplot(data=prob_frame, aes(x=consec, y=Prob, colour=factor(m))) +
  scale_colour_hue(h = c(250, 0)) + 
  geom_line(size=2.5) + geom_point(size=8) +
  #geom_hline(yintercept=0.9, linetype="dashed",size=2.5) +
  labs(x = expression(paste("The number of consecutive graders ", italic(l))), y = "Pr(Realizing a valid Y)", colour = "m") +
  theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black")) +
  theme(axis.text.x=element_text(size=30))+
  theme(axis.text.y=element_text(size=30))+
  theme(axis.title.x =element_text(hjust = 0.5,size=30))+
  theme(axis.title.y =element_text(hjust = 0.5,size=30)) +
  theme(legend.text=element_text(size=30), legend.title = element_text(size = 36, face ="bold"))
#guides(shape = guide_legend(reverse = TRUE))
gsubset_compare
dev.off()

