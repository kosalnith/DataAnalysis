# Data Visualization in R Programming: A practical introduction

## Create a label to install package together at the same time.
my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
                 "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                 "here", "interplot", "margins", "maps", "mapproj",
                 "mapdata", "MASS", "quantreg", "rlang", "scales",
                 "survey", "srvyr", "viridis", "viridisLite", "fs", "devtools")

install.packages(my_packages, repos = "http://cran.rstudio.com")
## R Studio should then download and install these packages for you. 


## To install the development version of socviz, instead of install.packages("socviz") do the following:
  
install.packages("socviz")

devtools::install_github("kjhealy/socviz")


library(socviz)

setup_course_notes(folder = "~/Desktop")

setup_course_notes(folder = "~/Documents")

url <- "https://cdn.rawgit.com/kjhealy/viz-organdata/master/organdonation.csv"
organs <- read_csv(file = url)
organs
library(gapminder)
gapminder

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

