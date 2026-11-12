rm(list = ls())
setwd("./LeftCensored/smy_files/")

## Create liikelihood function
dlnorm_Lm<-function(paras = numeric(), x = numeric(), xd = NA, w0 = NA, n0 = NA, n1 = NA){
  mu<-paras[1] 
  sig<-paras[2]
  if(length(paras)==3){
    w<-paras[3]  #mixtrue model include 3 parameter 
    f=n0*log(w+(1-w)*plnorm(xd,mu,sig))+n1*log(1-w)+sum(log(dlnorm(x,mu,sig)))
    #3 parameter censored model log-likelihood function
  }else{
    f=n0*log(w0+(1-w0)*plnorm(xd,mu,sig))+n1*log(1-w0)+sum(log(dlnorm(x,mu,sig)))
  }
  return(f) #return log-likelihood value
}

bootstrap_dlnorm <- function(rep_times = 100,sample_x = numeric(),xd = NA,sample_n = NA,
                             tmp_pc_step = 0.001,tol_err = 1e-6,method = "Nelder-Mean"){
  res_merge = data.frame()
  res_base = list()
  for(i in 1:rep_times){
    x = sample(sample_x, sample_n)
    if(pc==0){
      n0 = length(which(x <= xd))
      tx = x[which(x>xd)]
    }else{
      n0 = length(which(x < xd))
      tx = x[which(x>=xd)]
    }
    n1 = length(tx)
    
    res_base <- tryCatch({
      tmp = optim(
        dlnorm_Lm,
        x = tx,
        xd = xd,
        n0 = n0,
        n1 = n1,
        par = c(mean(tx), sd(tx), n0 / 2 / sample_n),
        method = "L-BFGS-B",
        lower = c(mean(tx)*0.5, sd(tx)*0.5, 0), upper = c(mean(tx)*1.5, sd(tx)*1.5, n0 / sample_n),
        control = list(fnscale = -1)
      )
      if(tmp$convergence!=0){
        NULL
      }else{
        tmp
      }
    }, error = function(e) {
      print(paste("fail to optim0:", e, sep = " "))
      NULL
    })
    
    if(!is.null(res_base)){
      ref_optim = data.frame(index = i,optim_mu = res_base$par[1],optim_sig = res_base$par[2],optim_w = res_base$par[3],
                             optim_loglike = res_base$value,optim_pc = plnorm(xd,res_base$par[1],res_base$par[2]))
      if(res_base$par[3]<0){
        tol_err = 0.1
      }
    }else{
      ref_optim = data.frame(index = i,optim_mu = NA,optim_sig = NA,optim_w = NA,
                             optim_loglike = NA,optim_pc = NA)
    }
    
    loglike_H0 = NA
    H0_mu = NA
    H0_sig = NA
    res1 = list()
    res1 <- tryCatch({
      tmp = list()
      for(meth in c("Nelder-Mead", "BFGS", "CG", "SANN")){
        tmp = optim(
          dlnorm_Lm,
          x = tx,
          xd = xd,
          n0 = n0,
          n1 = n1,
          w0 = 0,
          par = c(mean(tx), sd(tx)),
          method = meth,
          control = list(fnscale = -1)
        )
        if(tmp$convergence!=0){
          next()
        }else{
          break()
        }
      }
      if(tmp$convergence!=0){
        NULL
      }else{
        tmp
      }
    }, error = function(e) {
      print(paste("fail to optim1:", e, sep = " "))
      NULL
    })
    
    if(!is.null(res1)){ # H0: w = 0
      loglike_H0 = res1$value
      H0_mu = res1$par[1]
      H0_sig = res1$par[2]
    }
    
    res_optim = data.frame()
    res_optim_tmp = data.frame()
    
    ref_loglike = -Inf
    lambda = n0/sample_n+tmp_pc_step
    tmp_pc = 0
    k = 1
    while(tmp_pc <= lambda){
      w0 = (n0-sample_n*tmp_pc)/(sample_n*(1-tmp_pc))
      res = list()
      res <- tryCatch({
        tmp = optim(
          dlnorm_Lm,
          x = tx,
          xd = xd,
          n0 = n0,
          n1 = n1,
          w0 = w0,
          par = c(mean(tx), sd(tx)),
          method = method,
          control = list(fnscale = -1)
        )
        if(tmp$convergence!=0){
          NULL
        }else{
          tmp
        }
      }, error = function(e) {
        print(paste("fail to optim2:", e, sep = " "))
        NULL
      })
      
      if(!is.null(res)){
        Fxd = plnorm(xd,res$par[1],res$par[2])
        Pred_w = (n0-sample_n*Fxd)/(sample_n*(1-Fxd))
        ref_lambda = abs(Fxd + Pred_w - n0/sample_n)
        
        # H1: w > 0
        loglike_H1 = dlnorm_Lm(paras = c(res$par,Pred_w),x = tx,xd = xd,n0 = n0,n1 = n1)
        
        res_optim_tmp = rbind(res_optim_tmp,data.frame(index_k = k, tmp_pc = Fxd, err_lambda = ref_lambda,logLike = loglike_H1,
                                                       H0_mu = H0_mu, H0_sig = H0_sig,loglike_H0 = loglike_H0,LR = -2*(loglike_H0-loglike_H1),
                                                       Pred_mu = res$par[1], Pred_sig = res$par[2], Pred_w = Pred_w))
        
        if(res$value - ref_loglike < tol_err){
          break()
        }else{
          if(res$value > ref_loglike){
            ref_loglike = res$value
            tmp_pc = Fxd
          }else{
            tmp_pc = tmp_pc + tmp_pc_step
          }
        }
      }else{
        tmp_pc = tmp_pc + tmp_pc_step
      }
      k = k + 1
    }
    res_optim_tmp = res_optim_tmp[res_optim_tmp$Pred_w >= 0 & res_optim_tmp$Pred_w <= n0/sample_n,]
    if(nrow(res_optim_tmp)>0){
      res_optim = res_optim_tmp[which.max(res_optim_tmp$logLike),]
    }else{
      if(!is.null(res_base)){
        res_optim = data.frame(index_k = NA, tmp_pc = NA, err_lambda = NA,logLike = res_base$value,
                               H0_mu = H0_mu, H0_sig = H0_sig,loglike_H0 = loglike_H0,
                               LR = -2*(loglike_H0-res_base$value),
                               Pred_mu = res_base$par[1], Pred_sig = res_base$par[2], Pred_w = res_base$par[3])
      }else{
        res_optim = data.frame(index_k = NA, tmp_pc = NA, err_lambda = NA,logLike = NA,
                               H0_mu = H0_mu, H0_sig = H0_sig,loglike_H0 = loglike_H0,LR = NA,
                               Pred_mu = NA, Pred_sig = NA, Pred_w = NA)
      }
    }
    res_merge_tmp = cbind(ref_optim,res_optim)
    res_merge_tmp$censored_rate = n0/sample_n
    if(nrow(res_merge_tmp)>0){
      res_merge = rbind(res_merge,res_merge_tmp)
    }
  }
  return(res_merge)
}

args <- commandArgs(T)
pw_k = as.numeric(args[1])
pw_list = seq(0,0.9,by = 0.005)
pw = pw_list[pw_k]

method = "BFGS"
sample_n = 300
sample_mu = 1
sample_sig = 1
base_chisq = qchisq(0.9,df = 1)
smy_tot = data.frame()

# Create sample pool (size: 10000)
seed_id = 100
set.seed(seed_id+pw*100)
pool_X = rlnorm(10000,mean = sample_mu,sd = sample_sig)
seed_id = seed_id+1
set.seed(seed_id)
xdata = pool_X
if(pw > 0){
  zero_id = sample(1:10000,10000*pw)
  xdata[zero_id] = 0
}
sample_x = xdata

pro_tmp = seq(0.01,2,by=0.01)#c(2,3/2,1,1/2)
if(pw == 0){
  censored_rate = pc_list = seq(0.01,0.9,by=0.01)
}else{
  pro = pro_tmp[which(pro_tmp<=0.95/pw-1)]
  censored_rate = pw*(pro+1)
  pc_list = pw*pro
}
for(pc in pc_list){
  if(pc == 0){
    xd = 0
  }else{
    xd = qlnorm(pc,sample_mu,sample_sig)
  }
  tmp_tot = bootstrap_dlnorm(rep_times = 1000,sample_x = sample_x,sample_n = sample_n,xd = xd,method = "BFGS")
  tmp_tot$pw = pw
  tmp_tot$pc = pc
  pro_outliers = length(which(tmp_tot$optim_w<0))/nrow(tmp_tot)
  pro_failures = length(which(is.na(tmp_tot$optim_w)))/nrow(tmp_tot)
  
  power_rejectH0 = pred_w =CI_lr = CI_up = NA
  test_tmp = tmp_tot[!is.na(tmp_tot$LR),]
  if(nrow(test_tmp)>0){
    power_rejectH0 = length(which(test_tmp$LR>=base_chisq))/nrow(test_tmp)
    pred_w = mean(test_tmp$Pred_w)
    CI_lr = quantile(test_tmp$Pred_w,probs = 0.025)
    CI_up = quantile(test_tmp$Pred_w,probs = 0.975)
  }else{
    pred_w = mean(tmp_tot$Pred_w)
    CI_lr = quantile(tmp_tot$Pred_w,probs = 0.025)
    CI_up = quantile(tmp_tot$Pred_w,probs = 0.975)
  }
  
  tmp_smy = data.frame(pw, pc, pro_outliers, pro_failures, power_rejectH0, pred_w, CI_lr, CI_up)
  smy_tot = rbind(smy_tot,tmp_smy)
}
write.csv(smy_tot,paste("smy_tot",pw_k,".csv",sep = ""),row.names = F)
