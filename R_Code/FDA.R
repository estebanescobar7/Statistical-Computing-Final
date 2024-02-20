

library(tidyverse) # package for data managment
library(ggplot2) # package for plots
library(fda) # package for functional data analyis example
library(patchwork) # package for plot placement
library(reshape2) # pakcage for reshapeing data


source("original_paper_code/funs_2DFPCA.R")

# -----------------------
# ----- Example ---------
# -----------------------

set.seed(1644927850)

n_obs <- 80
time_span <- 100
time <- sort(runif(n_obs,0,time_span))
Wiener <- cumsum(rnorm(n_obs)) / sqrt(n_obs)
y_obs <- Wiener + rnorm(n_obs,0,.05)

p1 <- ggplot() +
  aes(x = time, y = y_obs) +
  geom_line() +
  geom_point( color = "red") +
  xlab('Time') +
  ylab('Observations')+
  ggtitle(" ") +
  theme_minimal()

times_basis = seq(0,time_span,1)
knots    = c(seq(0,time_span,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: cubic bspline: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(c(min(times_basis),max(times_basis)),n_basis,n_order,knots)
PHI = eval.basis(time, basis)

Wiener_obj <- smooth.basis(argvals = time, y = y_obs, fdParobj = basis); Wiener_obj
curve <- Wiener_obj$fd; curve
t <- seq(min(time),max(time),0.1)
yhat = predict(curve, newdata=t)

p2 <- ggplot() +
  geom_line( aes(x = t, y = yhat), col = "blue" ) +
  geom_line(aes(x = time, y = y_obs), alpha = 0.10  ) +
  geom_point(aes(x = time, y = y_obs), color = "red") +
  xlab('Time') +
  ylab('f(t)')+
  ggtitle(" ") +
  theme_minimal()

p1 | p2

ggsave("fda.png", plot = (p1 | p2), path = here::here("esteban_code/Plots"), dpi = "retina", width = 15, height = 10  )

# -----------------------
# ----- Example ---------
# -----------------------

# Function to simulate data
fake_curves <- function(n_curves = 100, n_points = 80, max_time = 100){
  ID <- 1:n_curves
  x <- vector(mode = "list", length = n_curves)
  t <- vector(mode = "list", length = n_curves)

  for (i in 1:n_curves){
    t[i] <- list(sort(runif(n_points,0,max_time)))
    x[i] <- list(cumsum(rnorm(n_points)) / sqrt(n_points))
  }
  df <- tibble(ID,t,x)
  names(df) <- c("ID", "Time", "Curve")
  return(df)
}

n_curves <- 10
n_points <- 80
max_time <- 100

df <- fake_curves(n_curves = n_curves,n_points = n_points, max_time = max_time)

df_1 <- df %>% dplyr::select(!c(ID,Curve)) %>% unnest_longer(Time)
df_2 <- df %>% dplyr::select(!c(ID,Time)) %>% unnest_longer(Curve)
ID <- sort(rep(1:n_curves,n_points))
df_l <- cbind(ID,df_1,df_2)

df_l <- df_l %>%
  mutate( ID = factor(ID, levels = 1:length(unique(ID)) )  )

p1 <- ggplot(df_l,
       aes(x = Time,
           y = Curve,
           color = ID,
           group = ID  ) ) +
  geom_line( alpha = 0.50) +
  geom_point(alpha = 0.50) +
  labs( x = "Time",
        y = "Observation",
        title = " ") +
  theme_minimal() +
  scale_color_viridis_d()

knots    = c(seq(0,max_time,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,max_time), n_basis)

argvals <- matrix(df_l$Time, nrow = n_points, ncol = n_curves)
y_mat <- matrix(df_l$Curve, nrow = n_points, ncol = n_curves)

W.obj <- Data2fd(argvals = argvals, y = y_mat, basisobj = basis, lambda = 0.5)

W_mean <- mean.fd(W.obj)
W_sd <- std.fd(W.obj)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(n_curves)
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(n_curves)

t <- seq(min(time),max(time),0.1)
X <- predict(W.obj, newdata=t)
ci.high <- predict(SE_u, newdata= t)
ci.low <- predict(SE_l, newdata = t)
average <- predict(W_mean, newdata = t )

CI <- data.frame(t = t, ci.high = ci.high , ci.low = ci.low, average = average  )

data <- data.frame( t, X )
colnames(data) <- c("t", 1:ncol(X))
data.long <- melt(data, id.vars = "t", variable.name = "ID"  )

 p2 <- ggplot(  ) +
  geom_line( aes(x = data.long$t, y = data.long$value, color = data.long$ID, group = data.long$ID), alpha = 0.50) +
  geom_line( aes(x= df_l$Time, y = df_l$Curve, group = df_l$ID), alpha = 0.10  ) +
  geom_point( aes(x = df_l$Time, y = df_l$Curve, group= df_l$ID), alpha= 0.10 ) +
  geom_line( aes(x = CI$t, y = CI$mean  ), linetype = "dashed"  ) +
  geom_line( aes(x = CI$t, y = CI$mean.2)) +
  geom_line( aes(x = CI$t, y = CI$mean.1 ), linetype = "dashed" )+
  labs( x = "Time",
        y = "Curve",
        color = "ID",
        title = " ") +
  theme_minimal() +
  scale_color_viridis_d(); p2


p1 | p2

ggsave("fda2.png", plot = (p1 | p2), path = here::here("esteban_code/Plots"), dpi = "retina", width = 20, height = 10  )



