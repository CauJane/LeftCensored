rm(list = ls())
# load("~/Desktop/左删失/article_leftCensored/multiDLs/lnorm_1_1_env.RData")
setwd("~/Desktop/左删失/article_leftCensored/20240718/simulations/")
library(ggplot2)
library(patchwork)
library(openxlsx)
library(reshape2) #数据处理相关
library(ggplot2) # 绘图相关
library(grDevices) #绘图颜色相关
library(RColorBrewer)#绘图颜色相关
library(directlabels) #等高线相关

## 2: Summary of simulation
## 241030
pathroot = "/Users/lihua/Desktop/左删失/article_leftCensored/20240718/simulations/ret_241030/"
fname_list <- dir(pathroot, full.names = T, pattern = "\\.csv",recursive = TRUE,include.dirs = TRUE)
pdata_ori = data.frame()
for(fname in fname_list){
  tmp = read.csv(fname,header = T)
  pdata_ori = rbind(pdata_ori,tmp)
}
pdata_ori$errorType1 = ifelse(pdata_ori$pw==0,pdata_ori$power_rejectH0,NA)
pdata_ori$errorType2 = ifelse(pdata_ori$pw==0,NA,1-pdata_ori$power_rejectH0)
pdata_ori$censored_rate = pdata_ori$pw+pdata_ori$pc
pdata_ori$power = ifelse(pdata_ori$pw==0,1-pdata_ori$power_rejectH0,pdata_ori$power_rejectH0)
pdata_ori$pc_pw = ifelse(pdata_ori$pw==0,NA,pdata_ori$pc/pdata_ori$pw)

set_box = seq(0,1,by = 0.1)
pdata_ori$group = rep(NA,nrow(pdata_ori))
pdata_ori$group[which(pdata_ori$pw==0)] = 0
pdata_ori$group[which(pdata_ori$pw!=0)] = sapply(pdata_ori$pw[which(pdata_ori$pw!=0)],
                                                 function(x){min(which(set_box>=x))})

p_list = list()
for(p_id in 1:length(unique(pdata_ori$group))){
  tmp_gr = sort(unique(pdata_ori$group))[p_id]
  expr_title = NA
  if(tmp_gr==0){
    expr_title = expression(omega==0)
  }else if(tmp_gr==2){
    expr_title = expression(omega %in% '(0,0.1)')
  }else{
    pw_lr = set_box[tmp_gr-1]
    pw_up = set_box[tmp_gr]
    expr_title = bquote(omega %in% '['~.(pw_lr)~','~.(pw_up)~')')
  }
  pdata = pdata_ori[which(pdata_ori$group==tmp_gr),c("censored_rate","pw","pred_w","CI_lr","CI_up")]
  if(tmp_gr!=0){
    pdata = aggregate(. ~ pw,data = pdata,FUN = mean)
  }
  p <- ggplot(data = pdata) +
    geom_ribbon(aes(x = censored_rate,ymin=CI_lr,ymax=CI_up),color="lightgrey",alpha=0.35)+
    geom_line(aes(x = censored_rate,y = pred_w)) +
    geom_point(aes(x = censored_rate,y = pred_w), pch = 16) +
    geom_line(aes(x = censored_rate,y = pw),lty=2,color="red") + #(1-power_rejectH0)
    labs(title = expr_title,x="Total censored rate", y=expression(omega))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
          panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = NA))
  p_list[[p_id]] = p
}
# p_list[[12]] = ggplot(data = data.frame()) + geom_point() + xlim(0, 1) + ylim(0, 1) + 
#   geom_segment(aes(x=0.2,y=0.65,xend=0.3,yend=0.65),color="red",lty=2)+
#   geom_segment(aes(x=0.2,y=0.5,xend=0.3,yend=0.5),color="black")+
#   geom_point(aes(x=0.25,y=0.5))+
#   geom_ribbon(data = data.frame(x=c(0.2,0.3),ymin=c(0.3,0.3),ymax=c(0.4,0.4)),aes(x=x,ymin=ymin,ymax=ymax),color="lightgrey",alpha=0.35)+
#   annotate("text",x=0.35,y=0.65,label=expression("Real value"~omega),size=3,hjust = 0)+
#   annotate("text",x=0.35,y=0.5,label=expression("Estimated value"~omega^"*"),size=3,hjust = 0)+
#   annotate("text",x=0.35,y=0.35,label="95% CI",size=3,hjust = 0)+
#   theme_bw()+
#   theme(panel.background = element_blank(),panel.border = element_blank(),
#         axis.text = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),
#         panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = NA))

ggsave("Simu_w.eps",p_list[[1]]+p_list[[2]]+p_list[[3]]+p_list[[4]]+p_list[[5]]+p_list[[6]]+p_list[[7]]+p_list[[8]]+p_list[[9]]+p_list[[10]],
       width = 10,height = 6)

pdata_type1 = pdata_ori[which(!is.na(pdata_ori$errorType1)),c("censored_rate","errorType1")]
tmp_type2 = pdata_ori[which(!is.na(pdata_ori$errorType2)),c("censored_rate","errorType2")]
censored_box = seq(min(tmp_type2$censored_rate),max(tmp_type2$censored_rate)+0.01,by = 0.01)
tmp_type2$level = sapply(tmp_type2$censored_rate,function(x){min(which(censored_box>=x))})
pdata_type2 = aggregate(. ~ level,data = tmp_type2,FUN = mean)
pdata_type2 = pdata_type2[order(pdata_type2$censored_rate),]
p_errType <- ggplot() +
  geom_line(data = pdata_type1,aes(x = censored_rate,y = errorType1)) +
  geom_line(data = pdata_type2,aes(x = censored_rate,y = errorType2),color="red") +
  labs(x="Total censored rate", y="Error rate")+
  ylim(c(0,1))+
  geom_segment(aes(x=0.01,y=0.95,xend=0.05,yend=0.95),color="black")+
  geom_segment(aes(x=0.01,y=0.85,xend=0.05,yend=0.85),color="red")+
  annotate("text",x=0.15,y=0.95,label="Type I error",size=3.5)+
  annotate("text",x=0.15,y=0.85,label="Type II error",size=3.5)+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = NA),
        axis.text = element_text(size=15,face="plain",color="black"),
        axis.title = element_text(size=18,face="plain",color="black"))

breaks_lines = quantile(pdata_ori$power)
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
Contour <- ggplot(pdata_ori,aes(x=pw,y=pc_pw,z=power))+#
  geom_tile(aes(fill=power))+
  scale_fill_gradientn(colours=colormap)+
  geom_contour(color="black")+#breaks=breaks_lines, expression(frac('p'['c'],'p'[omega]))
  labs(x=expression('p'[omega]),y=expression(frac('p'['c'],'p'[omega])),fill="Power")+
  theme_bw()+
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = NA),
    axis.text = element_text(size=15,face="plain",color="black"),
    axis.title.x = element_text(size=18,face="plain",color="black"),
    axis.title.y = element_text(size=18,face="plain",color="black",angle = 0.1,hjust = 0.5,vjust = 0.5),
    legend.title = element_text(size=11,face="plain",color="black"),
    legend.text = element_text(size=9,face="plain",color="black"),
    legend.background = element_blank(),
    legend.position = c(0.9,0.75) #"right" #
  )
ggsave("Simu_smy.eps",p_errType+Contour+plot_annotation(tag_levels = "A"),
       width = 10,height = 6)
save.image(file = "env_simulation.RData")

## Examples
rm(list = ls())
setwd("~/Desktop/左删失/article_leftCensored/20240718/simulations/")

## EM algorithm
##创建对数似然函数
dlnorm_Lm<-function(paras = numeric(), x = numeric(), xd = NA, w0 = NA, n0 = NA, n1 = NA){ #零膨胀比例不为0
  mu<-paras[1] 
  sig<-paras[2]
  if(length(paras)==3){
    w<-paras[3]  #mixtrue model include 3 parameter 
    f=n0*log(w+(1-w)*plnorm(xd,mu,sig))+n1*log(1-w)+sum(log(dlnorm(x,mu,sig)))
    #3 parameter censored model log-likelihood function
  }else{
    # if(w0 == 0){
    #   f=sum(log(dlnorm(x,mu,sig)))
    # }else{
    #   f=n0*log(w0+(1-w0)*plnorm(xd,mu,sig))+n1*log(1-w0)+sum(log(dlnorm(x,mu,sig)))
    # }
    f=n0*log(w0+(1-w0)*plnorm(xd,mu,sig))+n1*log(1-w0)+sum(log(dlnorm(x,mu,sig)))
  }
  return(f) #return log-likelihood value
}

EM_estFun <- function(x = numeric(), xd = NA, sample_n = sample_n,
                      tol_err = 1e-6, method = "Nelder-Mean", tmp_pc_step = 0.001){
  res_base = list()
  tx = x[which(x>=xd)]
  n1 = length(tx)
  n0 = sample_n - n1
  
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
    ref_optim = data.frame(optim_mu = res_base$par[1],optim_sig = res_base$par[2],optim_w = res_base$par[3],
                           optim_loglike = res_base$value,optim_pc = plnorm(xd,res_base$par[1],res_base$par[2]))
    if(res_base$par[3]<0){
      tol_err = 0.1
    }
  }else{
    ref_optim = data.frame(optim_mu = NA,optim_sig = NA,optim_w = NA,
                           optim_loglike = NA,optim_pc = NA)
  }
  
  loglike_H0 = NA
  H0_mu = NA
  H0_sig = NA
  res1 = list()
  res1 <- tryCatch({
    tmp = list()
    if(method == "L-BFGS-B"){
      tmp = optim(
        dlnorm_Lm,
        x = tx,
        xd = xd,
        n0 = n0,
        n1 = n1,
        w0 = 0,
        par = c(mean(tx), sd(tx)),
        method = method,
        lower = c(mean(tx)*0.5, sd(tx)*0.5), upper = c(mean(tx)*1.5, sd(tx)*1.5),
        control = list(fnscale = -1)
      )
    }else{
      for(meth in c("Nelder-Mead", "BFGS", "CG", "SANN")){
        tmp = list()
        tmp = optim(
          dlnorm_Lm,
          x = tx,
          xd = xd,
          n0 = n0,
          n1 = n1,
          w0 = 0,
          par = c(mean(tx), sd(tx)),
          control = list(fnscale = -1)
        )
        if(tmp$convergence!=0){
          next()
        }else{
          break()
        }
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
      if(method == "L-BFGS-B"){
        tmp = optim(
          dlnorm_Lm,
          x = tx,
          xd = xd,
          n0 = n0,
          n1 = n1,
          w0 = w0,
          par = c(mean(tx), sd(tx)),
          method = method,
          lower = c(mean(tx)*0.5, sd(tx)*0.5), upper = c(mean(tx)*1.5, sd(tx)*1.5),
          control = list(fnscale = -1)
        )
      }else{
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
      }
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
      
      res_optim_tmp = rbind(res_optim_tmp,data.frame(index = k, tmp_pc = Fxd, err_lambda = ref_lambda,logLike = loglike_H1,
                                                     H0_mu = H0_mu, H0_sig = H0_sig,loglike_H0 = loglike_H0,LR = -2*(loglike_H0-loglike_H1),
                                                     Pred_mu = res$par[1], Pred_sig = res$par[2], Pred_w = Pred_w))
      
      if(res$value - ref_loglike < tol_err){
        break()
      }else{
        if(res$value > ref_loglike){
          ref_loglike = res$value#
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
      res_optim = data.frame(index = NA, tmp_pc = NA, err_lambda = NA,logLike = res_base$value,
                             H0_mu = H0_mu, H0_sig = H0_sig,loglike_H0 = loglike_H0,
                             LR = -2*(loglike_H0-res_base$value),
                             Pred_mu = res_base$par[1], Pred_sig = res_base$par[2], Pred_w = res_base$par[3])
    }else{
      res_optim = data.frame(index = NA, tmp_pc = NA, err_lambda = NA,logLike = NA,
                             H0_mu = H0_mu, H0_sig = H0_sig,loglike_H0 = loglike_H0,LR = NA,
                             Pred_mu = NA, Pred_sig = NA, Pred_w = NA)
    }
  }
  res_merge = cbind(ref_optim,res_optim)
  res_merge$censored_rate = n0/sample_n
  return(res_merge)
}

data <- openxlsx::read.xlsx("~/Desktop/左删失/article_leftCensored/20240718/water_single.xlsx",sheet = 2,startRow = 2)
len<-length(data) #number of index in file_k
index<-colnames(data)
# type of pesticides: 1-forbidden; 2-others; 3-easily degraded
col_label = c(1,1,2,rep(1,8),3,1,1,1,3,2,3,2,3,2,1)

result = data.frame()
method = "BFGS"
## Limits of national standard
limits_vec = numeric()
tmp_limits = c(0.05,0.05,NA,0.01,0.05,0.05,NA,rep(0.01,4),NA,0.05,0.05,NA,0.1,rep(NA,6))
names(tmp_limits) = index
xd_vec = numeric()
for(i in 1:len){
  x <- as.numeric(as.vector(data[data[,i] != "ND" & data[,i] != "NODATA", i]))
  x = x[!is.na(x)]
  if(max(x)<1){
    max_time = 10^ceiling(log10(1/max(x))+1)#1/max(x)*10#
    x = x*max_time
  }
  xd = min(x[x!=0])
  n = length(!is.na(data[data[,i] != "NODATA",i]))
  
  tmp_res = EM_estFun(x = x,xd = xd,sample_n = n,method = method)
  limits_vec = append(limits_vec,ifelse(is.na(tmp_limits[i]),NA,tmp_limits[i]*max_time))
  xd_vec = append(xd_vec, xd)
  result = rbind(result,tmp_res)
}
names(limits_vec) = index
names(xd_vec) = index
rownames(result) = index
base_chisq = qchisq(0.9,df = 1)
result$w = ifelse(is.na(result$loglike_H0),result$Pred_w,
                  ifelse(result$LR>=base_chisq,result$Pred_w,0))
# result$H0_risk = plnorm(limits_vec,result$H0_mu,result$H0_sig,lower.tail = F)
# result$H1_risk = (1 - result$Pred_w) * plnorm(limits_vec,result$Pred_mu,result$Pred_sig,lower.tail = F)
result$H0_Fxd = plnorm(xd_vec,result$H0_mu,result$H0_sig,lower.tail = F)
result$H1_Fxd = (1 - result$Pred_w) * plnorm(xd_vec,result$Pred_mu,result$Pred_sig,lower.tail = F)
result$diff = abs(result$H0_Fxd - result$H1_Fxd)

col_label = col_label[order(result$w)]
result = result[order(result$w),]
result_sele = result[,c("censored_rate","H0_mu","H0_sig","loglike_H0","Pred_mu","Pred_sig","Pred_w","logLike","w")]

write.csv(result,"water_pesticides250709.csv",row.names = T)
#write.table(w_est,"est_w.txt")

## Fig of a certain sample
sele_sampe = "Parathion"
i = which(index==sele_sampe)
x <- as.numeric(as.vector(data[data[,i] != "ND" & data[,i] != "NODATA", i]))
x = x[!is.na(x)]
if(max(x)<1){
  max_time = 10^ceiling(log10(1/max(x))+1)#1/max(x)*10
  x = x*max_time
}
xd = min(x[x!=0])
n = length(!is.na(data[data[,i] != "NODATA",i]))
tx = x[which(x>=xd)]
n1 = length(tx)
n0 = n - n1
tmp_res = EM_estFun(x = x,xd = xd,sample_n = n,method = method)

# method of ggplot2
hist_x = hist(tx,breaks = seq(min(tx),max(tx),length.out = 6),plot = F)
bar_height = hist_x$counts/n

barData <- data.frame(x=c(0,hist_x$breaks),
                      probability=c(0,bar_height,0))

if(tmp_res$LR > base_chisq){
  cdf_pred = data.frame(x=c(seq(1e-3,max(x),length.out = 50)),
                        y=(1-tmp_res$Pred_w)*dlnorm(seq(1e-5,max(x),length.out = 50),tmp_res$Pred_mu,tmp_res$Pred_sig))
  Pred_w = tmp_res$Pred_w
}else{
  cdf_pred = data.frame(x=c(seq(1e-3,max(x),length.out = 50)),
                        y=dlnorm(seq(1e-5,max(x),length.out = 50),tmp_res$H0_mu,tmp_res$H0_sig))
  cdf_pred = cdf_pred[which(cdf_pred$y<1),]
  Pred_w = 0
}
p0 <- ggplot()+
  # geom_point(data = barData,aes(x=x,y=probability),pch=3)+
  geom_point(data = data.frame(x=x),aes(x=x,y=0),cex=3)+
  geom_bar(data = barData,aes(x=x,y=probability),stat="identity",just = 0,
           fill="transparent",color="black",width = c(diff(barData$x),0))+#diff(hist_x$breaks)[1]
  annotate("text",x=-1,y=0.06,label=expression("<ln("~x[d]~")"),hjust = 0,size=5)+
  annotate("text",x=0,y=0.02,label=bquote(omega^'*'==.(round(Pred_w,digits = 3))),hjust = 1,size=5)+
  geom_segment(aes(x=xd,y=0,xend=xd,yend=max(barData$probability)+0.05),lty=3)+
  geom_line(data = cdf_pred,aes(x=x,y=y),color="red")+
  xlim(c(-1.5,max(x)))+
  ylim(c(0,max(c(barData$probability,cdf_pred$y))+0.01))+
  geom_segment(
    aes(x = -0.5, y = 0.01, xend = 0, yend = 0),
    arrow = arrow(
      length = unit(0.02, "npc"), 
      type="closed" # Describes arrow head (open or closed)
    ))+
  theme_bw()+
  labs(x = paste0("Concentration ","(µg/L,1e",-log10(max_time),")"),color = "",y = "Density")+
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        axis.text = element_text(size=15,face="plain",color="black"),
        axis.title = element_text(size=18,face="plain",color="black"),
        panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = NA))
ggsave(paste0(sele_sampe,".eps"),p0,width = 10,height = 6)

arg_names = character()#gsub("\\."," ",rownames(result))
for(i in 1:length(rownames(result))){
  if(i == 19){
    arg_names_tmp = expression(beta~"-HCH")
  }else{
    arg_names_tmp = bquote(.(rownames(result)[i]))
  }
  arg_names = append(arg_names,arg_names_tmp)
}
setEPS()
postscript("example.eps",width = 8,height = 6)
par(mai = c(0.9, 1.7, 0.2, 0.2))
box_data = barplot(result$w,names.arg=arg_names,
                   col = sapply(col_label,function(x){ifelse(x==1,1,ifelse(x==2,1,8))}),
                   density = sapply(col_label,function(x){ifelse(x==1,0,ifelse(x==2,10,NA))}),
                   angle = sapply(col_label,function(x){ifelse(x==1,0,ifelse(x==2,30,45))}),
                   #ylab="Pesticides",space = 0.1,
                   las=2,cex.names = 0.8,xlab = NULL,xlim = c(0,1),horiz = T)
text(x=-0.25,y=median(box_data),xpd=T,srt=90,"Pesticides")
text(x=0.5,y=-4.5,xpd=T,
     expression("Zero exposure proportion"~(omega^'*')))
lines(result$diff, box_data, lty=3)
segments(x0=0.05,y0=box_data[1]-0.6,x1=0.05,y1=box_data[22]+0.6,col=2, lty=2)
text(0.3,box_data[2],expression(dF(X>=x[d])==0.05~"between "~H[0]~" and "~H[1]),cex = 0.8)
legend(0.8,box_data[6],
       legend = c("forbidden","easily degraded","others"),
       density = c(0,NA,20),angle = c(0,0,30),fill = c(1,8,1),
       cex = 0.7,bty = "n",col = 1)
legend(0.8,box_data[7]-0.3,legend = expression(dF(X>=x[d])),
       cex = 0.7,lty = 3,seg.len = 1,x.intersp = 0.59,bty = "n")
dev.off()
save.image(file = "env_example250709.RData")

