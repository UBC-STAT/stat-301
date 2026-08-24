knitr::opts_chunk$set(
  message = FALSE, 
  dev = "svg",
  fig.width = 5.33,
  fig.height = 3,
  fig.align = "center"
)
options(htmltools.dir.version = FALSE)
primary = "#89ABE3"
secondary = "c88b8b"
tertiary = "#0a8754"
fourth_color = "#a8201a"

## for simplicity
purple = primary
blue = primary
orange = secondary
green = tertiary
red = fourth_color

library(tidyverse)
library(fontawesome)
library(knitr)
theme_set(theme_bw(18))

pro = fa("thumbs-up", fill = green)
con = fa("bomb", fill = blue)

set.seed(12345)
