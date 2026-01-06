# This is the hmdcm package, developed in the following open access paper:
Liu, C.-W. (2026). Bayesian inference for dynamic Q-matrices and attribute trajectories in hidden Markov diagnostic classification models. British Journal of Mathematical and Statistical Psychology. https://doi.org/10.1111/bmsp.70028

# R Code for the Data Analyses of Problems in Elementary Probability Theory (same as Table 20)
1.	deps <- c("RcppTN","abind","clue","coda","doParallel","furrr","pgdraw","posterior","psych","mvtnorm","pks","edmdata")
2.	install.packages(setdiff(deps, rownames(installed.packages())))
3.	path <- "D:\\hmdcm_0.1.0.zip" # <--- modify the path on your end
4.	utils::install.packages(path, repos = NULL, type = "binary")
5.	library("pks"); data("probability"); probability <- as.data.frame(probability)
6.	f <- do.call(cbind, probability[,c("b101", "b102", "b103", "b104", "b105", "b106", "b107", "b108", "b109", "b110", "b111", "b112")])
7.	s <- do.call(cbind, probability[,c("b201", "b202", "b203", "b204", "b205", "b206", "b207", "b208", "b209", "b210", "b211", "b212")])
8.	keep <- rowSums(is.na(f))==0 & rowSums(is.na(s))==0
9.	f <- f[keep,,drop=FALSE]; s <- s[keep,,drop=FALSE]; res <- list(f, s)
10.	library("edmdata"); QQ <- rep(list(qmatrix_probability_part_one), 2)
11.	itemtype_string_list <- list(rep("SDCM",nrow(QQ[[1]])), rep("SDCM",nrow(QQ[[2]])))
12.	att_same_index <- list(); att_same_index[[1]] <- list(c(1,2,3,4),c(1,2,3,4))
13.	library("hmdcm"); a <-  hmdcm(res,K=sapply(QQ,ncol),itemtype_string=list(rep("SDCM",nrow(QQ[[1]])),rep("SDCM",nrow(QQ[[2]]))),identifiable_type=1,QQ_target=QQ,burnin=50000,keep=100000,beta_sd=c(2,1.5,0.5,0.5),nondecreasing=TRUE,att_same_index=att_same_index,nchains=2,para=T,rand.seed=1,equal_QQ=FALSE,equal_item=FALSE,equal_transition=FALSE)  
14.	a$est$attribute$time1$mean # attribute probability estimates at Time 1
15.	a$est$QQ_sample$time1$mean # Q probability estimates at Time 1
16.	a$est$class_prob$time1$mean # class proportion estimates at Time 1
17.	a$est$transition$time1$mean # transition matrix estimates at Time 1
18.	a$est$beta$time1$mean # item parameter estimates at Time 1
19.	a$est_full$eta$time1$mean # item response probability estimates at Time 1
20.	a$waic_overall$waic # WAIC
