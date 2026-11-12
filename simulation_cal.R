rm(list = ls())
setwd("/Users/lihua/Desktop/零膨胀多检出线/calculation/")
### 模拟计算
output_dir <- file.path(getwd(), "simulation")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
library(ggplot2)
library(patchwork)
library(dplyr)

# ============================================================
## 定义基础函数
# ============================================================
generate_sample <- function(N,omega,mu = 0,sigma = 1){
  z <- rbinom(N,1,1-omega)
  x <- numeric(N)
  x[z==1] <- rlnorm(sum(z==1),mu,sigma)
  x
}

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
## 0. 公共参数
# ============================================================
omega_true <- 0.10
mu <- 0
sigma <- 1
cost <- c(200,50)
F1_true = 0.10; F2_true = 0.30
d <- c(qlnorm(F1_true,mu,sigma), qlnorm(F2_true,mu,sigma))
tol_em = 1e-6 #EM估计值的容忍误差
max_iter <- 10000
n_repeat <- 1000
fix_F = FALSE

# ============================================================
## 3. 固定成本时，计算不同分配比例下的最大采样数量对估计值的影响
# ============================================================
set.seed(345)

cost_total = 4*10^4
n1_max = cost_total / cost[1]

results <- list(); k <- 1
for(n1 in seq(0,n1_max,by = n1_max/10)){
  n2_max = round((cost_total - cost[1]*n1) / cost[2])
  n2_list = unique(c(seq(0,n2_max,by = n1_max/10),n2_max))
  for(n2 in n2_list){
    N <- n1+n2
    for(rep in 1:n_repeat){
      x <- generate_sample(N,omega_true)
      m1 <- observe_method(x,n1,d[1])
      m2 <- observe_method(x,n2,d[2])
      r <- c(m1$r,m2$r)
      
      if(any(r == 0)) next
      n <- c(m1$n,m2$n)
      detected <- c(m1$detected,m2$detected)
      est <- estimate_F_omega(r,n,tol_em,fix_F = fix_F,d, F1_true, F2_true, detected = detected)
      results[[k]] <- data.frame(N=N,n1=n1,n2=n2,r1=r[1],r2=r[2],F1=est$F[1],F2=est$F[2],
                                 omega_hat=est$omega,mu_hat=est$mu,sig_hat=est$sig,
                                 cost=n1*cost[1]+n2*cost[2],iter=est$iter,
                                 abs_omega=abs(est$omega - omega_true),
                                 cost_pro=(n1*cost[1]+n2*cost[2])/cost_total)
      k <- k+1
      rm(list = c("x","m1","m2","r","n","detected","est"))
    }
  }
}

result3 <- do.call(rbind,results)

summary_result3 <- result3 %>%
  group_by(N, n1, n2, cost, cost_pro) %>%
  summarise(
    # 每种分配方式的数据量
    count = n(),
    
    # 估计偏差的均值和方差
    abs_omega_mean = mean(abs_omega, na.rm = TRUE),
    abs_omega_sd  = sd(abs_omega, na.rm = TRUE),
    
    # omega_hat 的均值和样本方差
    omega_mean = mean(omega_hat, na.rm = TRUE),
    omega_sd  = sd(omega_hat, na.rm = TRUE),
    
    # omega_hat 均值的 95%CI
    omega_ci_lower = quantile(omega_hat, 0.025, na.rm = TRUE),
    omega_ci_upper = quantile(omega_hat, 0.975, na.rm = TRUE),
    
    # mu_hat 的均值和样本方差
    mu_mean = mean(mu_hat, na.rm = TRUE),
    mu_sd  = sd(mu_hat, na.rm = TRUE),
    
    # mu_hat 均值的 95%CI
    mu_ci_lower = quantile(mu_hat, 0.025, na.rm = TRUE),
    mu_ci_upper = quantile(mu_hat, 0.975, na.rm = TRUE),
    
    # sig_hat 的均值和样本方差
    sig_mean = mean(sig_hat, na.rm = TRUE),
    sig_sd  = sd(sig_hat, na.rm = TRUE),
    
    # mu_hat 均值的 95%CI
    sig_ci_lower = quantile(sig_hat, 0.025, na.rm = TRUE),
    sig_ci_upper = quantile(sig_hat, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  )
# 目标优化函数
summary_result3$optim_fun = summary_result3$omega_sd*summary_result3$cost_pro+summary_result3$abs_omega_mean

# 如需导出 CSV 文件
write.csv(
  summary_result3,
  file.path(output_dir, "summary_result_fix_cost-261103.csv"),
  row.names = FALSE
)

## 画图代码
plot_tile_heatmap <- function(
  data,
  x_col,
  y_col,
  fill_col,
  
  # 坐标轴标题
  x_label = NULL,
  y_label = NULL,
  fill_label = NULL,
  
  # 颜色设置
  low_color = "#D73027",
  mid_color = "#FFFFBF",
  high_color = "#4575B4",
  na_color = "grey90",
  
  # 重复坐标的汇总方式
  aggregate_fun = "mean",
  
  # 坐标轴标签
  x_break_step = 1,
  y_break_step = 1,
  x_angle = 45,
  x_digits = NULL,
  y_digits = NULL,
  
  # 单元格设置
  tile_border = "white",
  tile_border_width = 0.2,
  
  # 其他
  title = NULL,
  base_size = 13
) {
  
  #=========================================================
  # 1. 检查输入数据
  #=========================================================
  
  if (!is.data.frame(data)) {
    stop("data 必须是 data.frame 或 tibble")
  }
  
  required_cols <- c(x_col, y_col, fill_col)
  
  if (!all(required_cols %in% names(data))) {
    stop(
      "以下列不存在：",
      paste(
        setdiff(required_cols, names(data)),
        collapse = ", "
      )
    )
  }
  
  if (!is.numeric(data[[fill_col]])) {
    stop("fill_col 必须为数值型变量")
  }
  
  if (!aggregate_fun %in%
      c("mean", "median", "min", "max")) {
    stop("aggregate_fun 只能为 mean、median、min、max")
  }
  
  if (x_break_step < 1 || y_break_step < 1) {
    stop("坐标轴标签间隔必须 >= 1")
  }
  
  #=========================================================
  # 2. 构建绘图数据
  #=========================================================
  
  df <- data.frame(
    x = data[[x_col]],
    y = data[[y_col]],
    z = data[[fill_col]]
  )
  
  df <- df[
    !is.na(df$x) &
      !is.na(df$y) &
      is.finite(df$z),
    ,
    drop = FALSE
    ]
  
  if (nrow(df) == 0) {
    stop("没有可用于绘图的有效数据")
  }
  
  #=========================================================
  # 3. 根据原始值升序排序
  #=========================================================
  
  x_values <- sort(unique(df$x))
  y_values <- sort(unique(df$y))
  
  nx <- length(x_values)
  ny <- length(y_values)
  
  #=========================================================
  # 4. 将实际值映射为等间隔坐标
  #=========================================================
  
  df$x_id <- match(df$x, x_values)
  df$y_id <- match(df$y, y_values)
  
  #=========================================================
  # 5. 汇总重复坐标组合
  #=========================================================
  
  fun <- switch(
    aggregate_fun,
    mean = mean,
    median = median,
    min = min,
    max = max
  )
  
  plot_df <- aggregate(
    z ~ x_id + y_id,
    data = df,
    FUN = fun
  )
  
  #=========================================================
  # 6. 坐标轴标签格式
  #=========================================================
  
  format_labels <- function(values, digits = NULL) {
    
    if (is.numeric(values) && !is.null(digits)) {
      return(format(
        round(values, digits),
        trim = TRUE,
        scientific = FALSE
      ))
    }
    
    as.character(values)
  }
  
  x_labels <- format_labels(x_values, x_digits)
  y_labels <- format_labels(y_values, y_digits)
  
  x_breaks <- seq(1, nx, by = x_break_step)
  y_breaks <- seq(1, ny, by = y_break_step)
  
  #=========================================================
  # 7. 设置默认标题
  #=========================================================
  
  if (is.null(x_label)) x_label <- x_col
  if (is.null(y_label)) y_label <- y_col
  if (is.null(fill_label)) fill_label <- fill_col
  
  #=========================================================
  # 8. 绘制热图
  #=========================================================
  
  p <- ggplot(
    plot_df,
    aes(
      x = x_id,
      y = y_id,
      fill = z
    )
  ) +
    
    geom_tile(
      width = 1,
      height = 1,
      color = tile_border,
      linewidth = tile_border_width
    ) +
    
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels[x_breaks],
      expand = c(0, 0)
    ) +
    
    scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels[y_breaks],
      expand = c(0, 0)
    ) +
    
    scale_fill_gradientn(
      colours = c(
        low_color,
        mid_color,
        high_color
      ),
      na.value = na_color,
      name = fill_label
    ) +
    
    coord_fixed(
      ratio = 1,
      xlim = c(0.5, nx + 0.5),
      ylim = c(0.5, ny + 0.5),
      expand = FALSE
    ) +
    
    labs(
      x = x_label,
      y = y_label,
      title = title
    ) +
    
    theme_classic(base_size = base_size) +
    
    theme(
      panel.grid = element_blank(),
      
      axis.text.x = element_text(
        angle = x_angle,
        hjust = ifelse(x_angle == 0, 0.5, 1)
      ),
      
      axis.ticks = element_blank(),
      
      legend.position = "right",
      
      legend.title = element_text(
        face = "plain"
      ),
      
      plot.title = element_text(
        hjust = 0.5
      )
    )
  
  return(p)
}

plot_data <- summary_result3 %>%
  group_by(N) %>%
  summarise(
    omega_mean = omega_mean[which.min(optim_fun)],
    omega_ci_lower = omega_ci_lower[which.min(optim_fun)],
    omega_ci_upper = omega_ci_upper[which.min(optim_fun)],
    cost_pro = cost_pro[which.min(optim_fun)],
    optim_fun = min(optim_fun, na.rm = TRUE),
    .groups = "drop"
  )

# 计算双轴的线性缩放参数
omega_range <- range(
  c(plot_data$omega_ci_lower,
    plot_data$omega_ci_upper,
    plot_data$omega_mean),
  na.rm = TRUE
)

sd_range <- range(plot_data$cost_pro, na.rm = TRUE)

# 避免范围为零时无法缩放
if (diff(omega_range) == 0 || diff(sd_range) == 0) {
  stop("omega_mean 或 omega_sd 的取值范围为零，无法进行双轴缩放。")
}

# 将 omega_sd 映射到 omega_mean 的坐标范围
scale_factor <- diff(omega_range) / diff(sd_range)

plot_data <- plot_data %>%
  mutate(
    sd_scaled = omega_range[1] +
      (cost_pro - omega_range[1]) * scale_factor
  )

p3_1 = ggplot(plot_data, aes(x = N)) +
  # omega_mean 的 95% 置信区间
  geom_ribbon(
    aes(
      ymin = omega_ci_lower,
      ymax = omega_ci_upper
    ),
    fill = "grey30",
    alpha = 0.18,
    na.rm = TRUE
  ) +
  # omega_mean 曲线
  geom_line(
    aes(y = omega_mean, color = "omega_mean"),
    linewidth = 1
  ) +
  geom_point(
    aes(y = omega_mean, color = "omega_mean"),
    size = 2
  ) +
  # 误差曲线
  geom_line(
    aes(y = sd_scaled, color = "omega_sd"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(
    aes(y = sd_scaled, color = "omega_sd"),
    size = 2
  ) +
  geom_vline(xintercept = plot_data$N[which.min(plot_data$optim_fun)], color="#E41A1C", lty = 2, lwd = 1) +
  scale_y_continuous(
    name = expression('Estimated zero-exposure proportion'~omega^'*'),
    sec.axis = sec_axis(
      ~ (. - omega_range[1]) / scale_factor + omega_range[1],
      name = "Cost ratio"
    )
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "omega_mean" = "black",
      "omega_sd" = "darkblue"
    ),
    breaks = c("omega_mean", "omega_sd"),
    labels = c(
      "omega_mean" = expression(omega^'*'),
      "omega_sd" = "Cost ratio"
    )
  ) +
  labs(
    x = "Total sample size (N)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.08,0.95),
    legend.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
print(p3_1)

ggsave(filename = file.path(output_dir, "EM-omega_by_size.jpg"),p3_1,
       width = 8, height = 3.5, dpi = 300)

p3_2 <- plot_tile_heatmap(
  data = summary_result3,
  x_col = "n2",
  y_col = "n1",
  fill_col = "optim_fun",
  
  x_label = expression('Sample size (n'[2]~')'),
  y_label = expression('Sample size (n'[1]~')'),
  fill_label = "",
  
  low_color = "#B2182B",
  mid_color = "#FEE08B",
  high_color = "#2166AC",
  na_color = "white",
  
  x_break_step = 5,
  y_break_step = 2,
  x_angle = 0
)
print(p3_2)

ggsave(filename = file.path(output_dir, "EM-omega_by_size_optim.jpg"),p3_2,
       width = 8, height = 3.5, dpi = 300)

# ============================================================
## 4. 给定分配比例（1:1）时，分析采样数量对估计值的影响
# ============================================================
set.seed(456)
N_step <- 50 # 样本量变化步长
N_list <- seq(50,2000,by=N_step)

results <- list(); k <- 1
for(N in N_list){
  n1 = N*0.5
  n2 = N*0.5
  for(rep in 1:n_repeat){
    x <- generate_sample(N = N,omega = omega_true,mu = mu,sigma = sigma)
    m1 <- observe_method(x,n1,d[1])
    m2 <- observe_method(x,n2,d[2])
    r <- c(m1$r,m2$r)
    
    if(any(r == 0)) next
    n <- c(m1$n,m2$n)
    detected <- c(m1$detected,m2$detected)
    est <- estimate_F_omega(r,n,tol_em,fix_F = fix_F,d, detected = detected)
    results[[k]] <- data.frame(N=N,r1=r[1],r2=r[2],F1=est$F[1],F2=est$F[2],
                               omega_hat=est$omega,mu_hat=est$mu,sig_hat=est$sig,
                               cost=n1*cost[1]+n2*cost[2],iter=est$iter)
    k <- k+1
    rm(list = c("x","m1","m2","r","n","detected","est"))
  }
}

result4 <- do.call(rbind,results)

summary_result4 <- result4 %>%
  group_by(N) %>%
  summarise(
    # 每种分配方式的数据量
    count = n(),

    # omega_hat 的均值和样本方差
    omega_mean = mean(omega_hat, na.rm = TRUE),
    omega_sd  = sd(omega_hat, na.rm = TRUE),
    
    # omega_hat 均值的 95%CI
    omega_ci_lower = quantile(omega_hat, 0.025, na.rm = TRUE),
    omega_ci_upper = quantile(omega_hat, 0.975, na.rm = TRUE),
    
    # mu_hat 的均值和样本方差
    mu_mean = mean(mu_hat, na.rm = TRUE),
    mu_sd  = sd(mu_hat, na.rm = TRUE),
    
    # mu_hat 均值的 95%CI
    mu_ci_lower = quantile(mu_hat, 0.025, na.rm = TRUE),
    mu_ci_upper = quantile(mu_hat, 0.975, na.rm = TRUE),
    
    # sig_hat 的均值和样本方差
    sig_mean = mean(sig_hat, na.rm = TRUE),
    sig_sd  = sd(sig_hat, na.rm = TRUE),
    
    # mu_hat 均值的 95%CI
    sig_ci_lower = quantile(sig_hat, 0.025, na.rm = TRUE),
    sig_ci_upper = quantile(sig_hat, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  )

plot_parameter_trends <- function(
  data,
  x_col = "N",
  # 显示真实值
  param_set = c(
    omega = omega_true,
    mu    = mu,
    sig   = sigma
  ),
  
  # 参数估计值列
  param_cols = c(
    omega = "omega",
    mu    = "mu",
    sig   = "sig"
  ),
  
  # 经验区间下限
  lower_cols = c(
    omega = "omega_ci_lower",
    mu    = "mu_ci_lower",
    sig   = "sig_ci_lower"
  ),
  
  # 经验区间上限
  upper_cols = c(
    omega = "omega_ci_upper",
    mu    = "mu_ci_upper",
    sig   = "sig_ci_upper"
  ),
  
  # 子图标题
  param_labels = c(
    omega = "Omega",
    mu    = "Mu",
    sig   = "Sigma"
  ),
  
  # 每个参数的颜色
  colors = c(
    omega = "#377EB8",
    mu    = "#E41A1C",
    sig   = "#4DAF4A"
  ),
  
  ribbon_alpha = 0.20,
  line_width = 0.9,
  show_points = TRUE,
  point_size = 1.8,
  base_size = 13,
  x_label = "Sample size (N)",
  ncol = 1
) {
  
  #========================================================
  # 1. 检查数据
  #========================================================
  
  if (!is.data.frame(data)) {
    stop("data 必须为 data.frame 或 tibble")
  }
  
  param_names <- names(param_cols)
  
  if (is.null(param_names) ||
      any(!nzchar(param_names))) {
    stop("param_cols 必须为带名称的字符向量")
  }
  
  if (!all(param_names %in% names(lower_cols)) ||
      !all(param_names %in% names(upper_cols))) {
    stop("参数名称与区间列名称不一致")
  }
  
  required_cols <- unique(c(
    x_col,
    unname(param_cols),
    unname(lower_cols[param_names]),
    unname(upper_cols[param_names])
  ))
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(
      "数据缺少以下列：",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!all(vapply(
    data[required_cols],
    is.numeric,
    logical(1)
  ))) {
    stop("样本量、估计值和经验区间列必须为数值型")
  }
  
  #========================================================
  # 2. 逐个参数绘图
  #========================================================
  
  plot_list <- lapply(param_names, function(param) {
    
    estimate_col <- unname(param_cols[param])
    lower_col <- unname(lower_cols[param])
    upper_col <- unname(upper_cols[param])
    true_value = param_set[param]
    
    plot_df <- data.frame(
      N = data[[x_col]],
      estimate = data[[estimate_col]],
      lower = data[[lower_col]],
      upper = data[[upper_col]]
    )
    
    # 剔除无效数据
    plot_df <- plot_df[
      complete.cases(plot_df) &
        is.finite(plot_df$N) &
        is.finite(plot_df$estimate) &
        is.finite(plot_df$lower) &
        is.finite(plot_df$upper),
      ,
      drop = FALSE
      ]
    
    if (nrow(plot_df) == 0) {
      stop(paste("参数", param, "没有有效数据"))
    }
    
    # 检查经验区间
    if (any(plot_df$lower > plot_df$upper)) {
      stop(paste("参数", param, "存在区间下限大于上限"))
    }
    
    # 按样本量升序排列
    plot_df <- plot_df[
      order(plot_df$N),
      ,
      drop = FALSE
      ]
    
    color <- if (param %in% names(colors)) {
      unname(colors[param])
    } else {
      "#377EB8"
    }
    
    label <- if (param %in% names(param_labels)) {
      unname(param_labels[param])
    } else {
      param
    }
    
    #======================================================
    # 3. 单参数子图
    #======================================================
    
    p <- ggplot(
      plot_df,
      aes(x = N, y = estimate)
    ) +
      
      geom_hline(yintercept = true_value, lty = 2, lwd = line_width) +
      
      geom_ribbon(
        aes(
          ymin = lower,
          ymax = upper
        ),
        fill = color,
        alpha = ribbon_alpha
      ) +
      
      geom_line(
        color = color,
        linewidth = line_width
      )
    
    if (show_points) {
      p <- p +
        geom_point(
          color = color,
          size = point_size
        )
    }
    
    p <- p +
      labs(
        x = x_label,
        y = label
      ) +
      theme_classic(base_size = base_size) +
      theme(
        panel.grid.major.y = element_line(
          color = "grey90",
          linewidth = 0.3
        ),
        panel.grid.minor = element_blank(),
        axis.title = element_text(
          face = "plain"
        )
      )
    
    return(p)
  })
  
  names(plot_list) <- param_names
  
  #========================================================
  # 4. 合并子图
  #========================================================
  
  combined_plot <- wrap_plots(
    plot_list,
    ncol = ncol
  )
  
  return(combined_plot)
}

p4 <- plot_parameter_trends(
  data = summary_result4,
  
  x_col = "N",
  
  param_cols = c(
    omega = "omega_mean",
    mu = "mu_mean",
    sig = "sig_mean"
  ),
  
  lower_cols = c(
    omega = "omega_ci_lower",
    mu = "mu_ci_lower",
    sig = "sig_ci_lower"
  ),
  
  upper_cols = c(
    omega = "omega_ci_upper",
    mu = "mu_ci_upper",
    sig = "sig_ci_upper"
  ),
  
  param_labels = c(
    omega = expression(omega^'*'),
    mu    = expression(mu^'*'),
    sig   = expression(sigma^'*')
  ),
  
  colors = c(
    omega = "#2166AC",
    mu = "#B2182B",
    sig = "#1B7837"
  )
)

print(p4)
ggsave(
  filename = file.path(output_dir, "parameter_trends.jpg"),
  plot = p4,
  width = 9,
  height = 6,
  dpi = 300
)
