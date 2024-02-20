

#library(tidyverse)
library(devtools) # these package gave me some trouble
library(ggbiplot) # this package also gave me some trouble
#install_github("vqv/ggbiplot")

set.seed(111)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,]

pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(pc)

g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top') + theme_minimal()

ggsave("pca.png", plot = g, path = here::here("esteban_code/Plots"), dpi = "retina", width = 15, height = 11  )

