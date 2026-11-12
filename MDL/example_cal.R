rm(list = ls())
setwd("/Pathway/to/save/")
### Calculation of example data
output_dir <- getwd()

# ============================================================
## Define basic functions
# ============================================================
observe_method <- function(x,n,d){
  y <- sample(x,n)
  list(n=n,r=sum(y<d),detected=y[y>=d])
}

get_initial_omega <- function(n, r, tol) {
  stopifnot(all(n > 0L), all(r >= 0L), all(r <= n))
  observed_nd <- r / n
  discrepancy <- ifelse(all(observed_nd <= tol), min(observed_nd), max(observed_nd - tol))
}

em_omega <- function(r,n,vec_F,tol,tol_em = 1e-6){
  M <- sum(n - r)
  if (M == 0L) return(list(omega=1,iter=0))
  
  # Avoid machine underflow when a trial (mu, sigma) is extreme.
  # 0 < F_j < 1
  vec_F <- pmin(pmax(vec_F, 1e-12), 1 - 1e-12)
  D0 <- sum(r * (1 - vec_F) / vec_F) - M
  if (D0 <= 0) return(list(omega=0,iter=0))
  
  N0 <- sum(n)
  omega <- get_initial_omega(n, r, tol)
  for(i in 1:max_iter){
    omega_new <- sum(r*omega/(vec_F+(1-vec_F)*omega))/N0
    if(abs(omega_new-omega)<tol_em) return(list(omega=omega_new,iter=i))
    omega <- omega_new
  }
  list(omega=omega,iter=max_iter)
}

loglik <- function(omega,r,n,vec_F,detected_C0){
  M <- sum(n-r)
  M*log(1-omega)+sum(r*log(vec_F+(1-vec_F)*omega))+detected_C0
}

estimate_F_omega <- function(r,n,tol_em,fix_F,d, mu_true, sig_true, detected, 
                             mu_lim = c(-1,1), sig_lim = c(0.1,2), tol = 0.1){
  best <- list(ll=-Inf)
  if(fix_F){
    F1_grid <- mu_true
    F2_grid <- sig_true
  }else{
    F1_grid <- seq(round(mu_lim[1],digits = log10(1/tol)),mu_lim[2],by=tol)
    F2_grid <- seq(round(sig_lim[1],digits = log10(1/tol)),sig_lim[2],by=tol)
  }
  ret_data = data.frame()
  for(mu in F1_grid){
    for(sig in F2_grid){
      vec_F <- plnorm(d, meanlog = mu, sdlog = sig)
      em <- em_omega(r,n,vec_F,tol = tol,tol_em)
      detected_C0 = sum(log(dlnorm(detected, meanlog = mu, sdlog = sig)))
      ll <- loglik(em$omega,r,n,vec_F,detected_C0)
      if(ll>best$ll) best <- list(F=vec_F,omega=em$omega,mu=mu,sig=sig,iter=em$iter,ll=ll)
    }
  }
  best
}

# ============================================================
## 0. Common parameters
# ============================================================
tol_em = 1e-6 #Tolerance for EM estimates
max_iter <- 10000
n_repeat <- 1000
fix_F = FALSE
##------- Bootstrap Resampling Analysis --------
bootstrap_data_list <- function(
  data_list,
  B = 1000,
  seed = NULL
) {
  
  #====================================================
  # 1. Check input data
  #====================================================
  
  if (!is.list(data_list) || length(data_list) == 0) {
    stop("data_list must be null list")
  }
  
  if (!is.numeric(B) ||
      length(B) != 1 ||
      !is.finite(B) ||
      B < 1 ||
      B != as.integer(B)) {
    stop("B must be a positive integer")
  }
  
  B <- as.integer(B)
  
  for (j in seq_along(data_list)) {
    
    group <- data_list[[j]]
    
    if (!all(c("n", "r", "detected") %in% names(group))) {
      stop(paste0("No ", j, " group miss required fields"))
    }
    
    n <- group$n
    r <- group$r
    detected <- group$detected
    
    if (length(n) != 1 ||
        length(r) != 1 ||
        !is.finite(n) ||
        !is.finite(r) ||
        n < 1 ||
        r < 0 ||
        r > n ||
        n != floor(n) ||
        r != floor(r)) {
      stop(paste0("No ", j, " group: n or r is illegal"))
    }
    
    if (!is.numeric(detected) ||
        any(!is.finite(detected))) {
      stop(paste0("No ", j, " group: detected is illegal"))
    }
    
    if (length(detected) != n - r) {
      stop(
        paste0(
          "Data in No ", j, " group is inconsistency：",
          "length(detected) != n - r"
        )
      )
    }
  }
  
  #====================================================
  # 2. Set the random seed
  #====================================================
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  #====================================================
  # 3. Single Bootstrap Resampling
  #====================================================
  
  bootstrap_once <- function(data_list) {
    
    result <- lapply(data_list, function(group) {
      
      n <- as.integer(group$n)
      r <- as.integer(group$r)
      detected <- group$detected
      
      # Undetected proportion of raw data
      p_nd <- r / n
      
      # Number of undetected resamples
      r_boot <- rbinom(
        n = 1,
        size = n,
        prob = p_nd
      )
      
      # Number of detections after resampling
      m_boot <- n - r_boot
      
      # Sampling with replacement from the initial detected concentrations
      if (m_boot > 0) {
        
        index <- sample.int(
          n = length(detected),
          size = m_boot,
          replace = TRUE
        )
        
        detected_boot <- detected[index]
        
      } else {
        
        detected_boot <- numeric(0)
        
      }
      
      # Preserve the original data structure
      list(
        n = n,
        r = as.integer(r_boot),
        detected = detected_boot
      )
    })
    
    return(result)
  }
  
  #====================================================
  # 4. Repeat B times
  #====================================================
  
  bootstrap_results <- replicate(
    n = B,
    expr = bootstrap_once(data_list),
    simplify = FALSE
  )
  
  return(bootstrap_results)
}

ana_bootstrap = function(data_list,d,n_repeat = 1000,seed = NULL){
  bootstrap_list = bootstrap_data_list(data_list,B = n_repeat,seed = seed)
  
  results <- list()
  for(k in 1:n_repeat){
    r = numeric()
    n = numeric()
    detected = numeric()
    for(j in 1:length(bootstrap_list[[k]])){
      r = append(r, bootstrap_list[[k]][[j]]$r)
      n = append(n, bootstrap_list[[k]][[j]]$n)
      detected = append(detected, bootstrap_list[[k]][[j]]$detected)
    }
    mu_x = mean(log(detected))
    sig_x = sd(log(detected))
    mu_lim = mu_x + c(-3,3)*sig_x
    sig_lim = c(0.1,2)*sig_x
    
    est <- estimate_F_omega(r,n,tol_em,fix_F = fix_F,d = d, detected = detected,
                            mu_lim = mu_lim, sig_lim = sig_lim)
    results[[k]] <- data.frame(No=k,omega_hat=est$omega,mu_hat=est$mu,sig_hat=est$sig,iter=est$iter)
    rm(list = c("mu_x","sig_x","mu_lim","sig_lim","est"))
  }

  result <- do.call(rbind,results)
  return(result)
}

##------- Analysis for combinations of multiple detection methods --------
ana_fun_meths = function(data_list,d){
  x = unlist(lapply(data_list,FUN = function(x){x$detected}))
  mu_x = mean(log(x)); sig_x = sd(log(x))
  mu_lim = mu_x + c(-3,3)*sig_x
  sig_lim = c(0.1,2)*sig_x
  results <- list(); k <- 1
  for(num_i in 1:length(data_list)){
    if(num_i == 1){
      for(i in 1:length(data_list)){
        ana_data = data_list[[i]]
        r <- ana_data$r
        n <- ana_data$n
        detected <- ana_data$detected
        est <- estimate_F_omega(r,n,tol_em,fix_F = fix_F,d = d[i], detected = detected,
                                mu_lim = mu_lim, sig_lim = sig_lim)
        results[[k]] <- data.frame(Name=paste0("M",i),
                                   Nums_of_Meths=num_i,d=as.character(d[i]),
                                   omega_hat=est$omega,mu_hat=est$mu,sig_hat=est$sig,iter=est$iter)
        k <- k+1
        rm(list = c("ana_data","r","n","detected","est"))
      }
    }else{
      comb_gr = combn(length(data_list),num_i)
      for(i in 1:ncol(comb_gr)){
        r = numeric()
        n = numeric()
        detected = numeric()
        for(j in 1:nrow(comb_gr)){
          r = append(r, data_list[[j]]$r)
          n = append(n, data_list[[j]]$n)
          detected = append(detected, data_list[[j]]$detected)
        }
        est <- estimate_F_omega(r,n,tol_em,fix_F = fix_F,d = d[comb_gr[,i]], detected = detected,
                                mu_lim = mu_lim, sig_lim = sig_lim)
        results[[k]] <- data.frame(Name=paste(paste0("M",comb_gr[,i]),collapse = "-"),
                                   Nums_of_Meths=num_i,d=paste(d[comb_gr[,i]],collapse = ","),
                                   omega_hat=est$omega,mu_hat=est$mu,sig_hat=est$sig,iter=est$iter)
        k <- k+1
        rm(list = c("r","n","detected","est"))
      }
    }
  }
  result <- do.call(rbind,results)
  return(result)
}

library(dplyr)
library(ggplot2)
# ================ Case Study: Detection of Chlorpyrifos =======================
data_ori = openxlsx::read.xlsx("SI_tables.xlsx", sheet = 1)
d_list = sort(unique(data_ori$`Detection.limit`),decreasing = T)
data_infos = data.frame(N=rep(nrow(data_ori),length(d_list)),
                        NumsOfFood=rep(length(unique(data_ori$Food)),length(d_list)),
                        NumsOfProv=rep(length(unique(data_ori$`Sampling.Unit`)),length(d_list)),
                        d_j=d_list)
d = numeric()
r_list = numeric()
n_list = numeric()
data_list = list(); k = 1
for(i in 1:length(d_list)){
  tmp_x = data_ori$`Concentration.(mg/kg)`[which(data_ori$`Detection.limit`==d_list[i])]
  n_list = append(n_list, length(tmp_x))
  r_list = append(r_list, length(which(tmp_x == "ND")))
  tmp_detected = tmp_x[which(tmp_x != "ND")]
  if(length(tmp_detected)!=0){
    data_list[[k]] = list(n=length(tmp_x),r=length(which(tmp_x == "ND")),
                          detected=as.numeric(tmp_detected))
    d = append(d, d_list[i])
    k = k + 1
  }else{
    if(i != 1){
      data_list[[k-1]]$n = data_list[[k-1]]$n + length(tmp_x)
      data_list[[k-1]]$r = data_list[[k-1]]$r + length(which(tmp_x == "ND"))
    }
  }
}
data_infos$r_j = r_list
data_infos$n_j = n_list
write.csv(data_infos, file.path(output_dir, "data_infos-Chlorpyrifos.csv"), row.names = F)

## calculate 95%CI
result_b1 <- ana_bootstrap(data_list = data_list, d = d, seed = 111)
omega_mean = mean(result_b1$omega_hat)
omega_lower = quantile(result_b1$omega_hat, probs = 0.025)
omega_upper = quantile(result_b1$omega_hat, probs = 0.975)
cat("omega^* = ",round(omega_mean,digits = 3),
    "; 95%CI: [",round(omega_lower,digits = 3),",",round(omega_upper,digits = 3),"]",sep = "")

## results of multiple detection limits in different combinations
result_1 <- ana_fun_meths(data_list = data_list, d = d)

# ================ Case Study: Detection of Lead =======================
data_ori = openxlsx::read.xlsx("SI_tables.xlsx", sheet = 2)
d_list = sort(unique(data_ori$`Detection.limit`),decreasing = T)
data_infos = data.frame(N=rep(nrow(data_ori),length(d_list)),
                        NumsOfFood=rep(length(unique(data_ori$Food)),length(d_list)),
                        NumsOfProv=rep(length(unique(data_ori$`Sampling.Unit`)),length(d_list)),
                        d_j=d_list)
d = numeric()
r_list = numeric()
n_list = numeric()
n_min = 30
data_list = list(); k = 1
for(i in 1:length(d_list)){
  tmp_x = data_ori$`Concentration.(mg/kg)`[which(data_ori$`Detection.limit`==d_list[i])]
  n_list = append(n_list, length(tmp_x))
  r_list = append(r_list, length(which(tmp_x == "ND")))
  
  if(length(tmp_x) < n_min) next
  tmp_detected = tmp_x[which(tmp_x != "ND")]
  if(length(tmp_detected)!=0){
    data_list[[k]] = list(n=length(tmp_x),r=length(which(tmp_x == "ND")),
                          detected=as.numeric(tmp_detected))
    d = append(d, d_list[i])
    k = k + 1
  }else{
    if(i != 1){
      data_list[[k-1]]$n = data_list[[k-1]]$n + length(tmp_x)
      data_list[[k-1]]$r = data_list[[k-1]]$r + length(which(tmp_x == "ND"))
    }
  }
}
data_infos$r_j = r_list
data_infos$n_j = n_list

write.csv(data_infos, file.path(output_dir, "data_infos-Lead.csv"), row.names = F)

## calculate 95%CI
result_b2 <- ana_bootstrap(data_list = data_list, d = d, seed = 222)
omega_mean = mean(result_b2$omega_hat)
omega_lower = quantile(result_b2$omega_hat, probs = 0.025)
omega_upper = quantile(result_b2$omega_hat, probs = 0.975)
cat("omega^* = ",round(omega_mean,digits = 3),
    "; 95%CI: [",round(omega_lower,digits = 3),",",round(omega_upper,digits = 3),"]",sep = "")

## results of multiple detection limits in different combinations
result_2 <- ana_fun_meths(data_list = data_list, d = d)

#----------- Plot codes -------------
plot_data <- result_1 %>%
  arrange(Nums_of_Meths, desc(omega_hat))
plot_data$Nums_of_Meths <- factor(
  plot_data$Nums_of_Meths,
  levels = unique(plot_data$Nums_of_Meths)
)
plot_data$Name <- factor(
  plot_data$Name,
  levels = unique(plot_data$Name)
)
p_hist <- ggplot(plot_data,
                 aes(x = Name, y = omega_hat, color = Nums_of_Meths)) +
  geom_segment(
    aes(
      xend = Name,
      y = 0,
      yend = omega_hat
    ),
    linewidth = 0.8
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    x = NULL,
    y = expression('Estimated zero-exposure proportion'~omega^'*'),
    color = "Nums of Methods"
  ) +
  scale_color_brewer(palette = "Set1") +
  guides(color=guide_legend(ncol = 3, byrow= TRUE )) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      # angle = 30,
      # hjust = 1,
      hjust = 0.5,
      size = 6.5, face = "bold"
    ),
    axis.title = element_text(
      face = "plain"
    ),
    # legend.position = c(0.8,0.8),
    legend.position = "top",
    legend.title = element_text(
      face = "plain"
    ),
    panel.grid = element_blank()
  )

# output
ggsave(
  "example_hist-Chlorpyrifos.jpg",
  p_hist,
  width = 5,
  height = 3.5,
  dpi = 300
)
ggsave(
  "example_hist-Lead.jpg",
  p_hist,
  width = 10,
  height = 4,
  dpi = 300
)

boxData = data.frame(omega = append(result_1$omega_hat,result_2$omega_hat),
                     label = rep(c("Chlorpyrifos","Lead"),times=c(nrow(result_1),nrow(result_2))))

p_box <- ggplot(
  boxData,
  aes(
    x = label,
    y = omega
  )
) +
  geom_boxplot() +
  geom_jitter(
    width = 0.15,
    height = 0
  ) +
  labs(
    x = NULL,
    y = expression('Estimated zero-exposure proportion'~omega^'*')
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      hjust = 0.5, face = "bold"
    ),
    axis.title = element_text(
      face = "plain"
    ),
    panel.grid = element_blank()
  )
ggsave(
  "example_box.jpg",
  p_box,
  width = 5,
  height = 3.5,
  dpi = 300
)



