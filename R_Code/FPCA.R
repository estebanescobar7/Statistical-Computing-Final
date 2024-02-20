
library(tidyverse) # package for data managment
library(ggplot2) # package for plots
library(fda) # package for functional data analyis example
library(patchwork) # package for plot placement
library(reshape2) # pakcage for reshapeing data

# France, total population, Deaths from 1816 - 2020
death <- read.table( here::here( "esteban_code/data/Deaths_1_by_1.txt"  ), header = TRUE )

death <- death %>%
  dplyr::select(Year,Age, Male  ) %>%
  rename(male.death = Male, year = Year, age.death = Age)  %>%
  mutate(year = as.numeric(year) ) %>%
  mutate(age.death = ifelse(age.death == "110+", 110, age.death  )  )  %>%
  mutate(age.death = as.numeric(age.death) )

dim(death) # 22755 by 3
summary(death)

# France, Total population, population size. from 1816 - 2021
population <- read.table( here::here( "esteban_code/data/Population.txt"  ), header = TRUE )

population <- population %>%
  dplyr::select(Year, Male  ) %>%
  rename(year = Year, male.total.pop = Male) %>%
  mutate(year = ifelse( str_detect(year, "[-]" ) == TRUE , NA, year )  ) %>%
  drop_na(year) %>%
  mutate( year = ifelse( str_detect(year, "[+]" ) == TRUE , gsub("[+]", '',year ), year ) ) %>%
  mutate(year = as.numeric(year) ) %>%
  filter(year < 2021 )

dim(population) # 22755 by 2
summary(population)

death <- as.data.frame(death)
population <- as.data.frame(population)

data.full <- cbind(death, male.total.pop = population$male.total.pop)

data.full <- data.full %>%
  filter(male.total.pop > 0 & male.death > 0 )

dim(data.full)
summary(data.full)

data.full <- data.full %>%
  mutate(mortality.rate = male.death/male.total.pop ) %>%
  mutate(log.mort.rate = log(mortality.rate) ) %>%
  mutate(year = factor(year) ) %>%
  filter(age.death <=100)

p1 <- ggplot(data.full,
             aes(x = age.death,
                 y = log.mort.rate,
                 color = year,
                 group = year  ) ) +
  geom_line( alpha = 0.50) +
  labs( x = "Age",
        y = "Log Mortality Rate",
        title = "French Male log Mortality Rate From 1816 - 2020") +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_color_viridis_d()

max_time <- max(data.full$age.death)
n_points <- 101
n_curves <- 205

knots    = c(seq(0,max_time,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,max_time), n_basis)

argvals <- matrix(data.full$age.death, nrow = n_points, ncol = n_curves)
y_mat <- matrix(data.full$log.mort.rate, nrow = n_points, ncol = n_curves)

W.obj <- Data2fd(argvals = argvals, y = y_mat, basisobj = basis, lambda = 0.5)

t <- seq(0,max_time,0.1)
X <- predict(W.obj, newdata=t)

data <- data.frame( t, X )
colnames(data) <- c("t", 1:ncol(X))
data.long <- melt(data, id.vars = "t", variable.name = "ID"  )

p2 <- ggplot(data.long) +
  aes(x = t, y = value, color = ID, group = ID) +
  geom_line(  alpha = 0.40) +
  labs( x = "Age",
        y = "Log Mortality Rate",
        title = "French Male log Mortality Rate From 1816 - 2020") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_viridis_d()

fun_pca <- pca.fd(W.obj, nharm = 2)
t <- seq(0,max_time,0.1)
curves <- fun_pca$harmonics
X <- predict(curves, newdata = t )
fun_pca_plot <- data.frame(t = t, X )
fun_pca_plot_long <- melt(fun_pca_plot, id.vars = "t", variable.name = "PC"  ); fun_pca_plot_long

p3 <- ggplot(fun_pca_plot_long) +
  aes(x = t, y = value, color = PC, group = PC) +
  geom_line() +
  labs( x = "Age",
        y = " ",
        title = "Functional Principal Components"
        ) +
  theme_minimal() +
  scale_color_viridis_d()

ggsave("fpca.png",
       plot = (p1 + p2 + p3 +  plot_layout(nrow = 2, ncol = 2, byrow = TRUE) ),
       path = here::here("esteban_code/Plots"),
       dpi = "retina",
       width = 15,
       height = 10  )




















