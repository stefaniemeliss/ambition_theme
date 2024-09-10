# ambition colours
navy    = "#474C68"
cyan    = "#14B4E9"
coral   = "#E94B58"
teal    = "#00987C"
purple  = "#6D2160"
orange  = "#EC642D"
yellow  = "#FFCC00"
blue    = "#006FB7"
red     = "#BF1C1D"
white   = "#FFFFFF"
black   = "#000000"

# function to create colour tints
# source: https://rdrr.io/cran/MESS/src/R/colorfunctions.R
col.tint <- function(col, tint=.4) {
  
  if(missing(col))
    stop("a vector of colours is missing")
  
  if (tint<0 | tint>1)
    stop("shade must be between 0 and 1")
  
  mat <- t(col2rgb(col, alpha=TRUE)  +  c(rep(1-tint, 3), 0)*(255-col2rgb(col, alpha=TRUE)))
  rgb(mat, alpha=mat[,4], maxColorValue=255)
}

# generate tints #

# navy
navy40 <- col.tint(navy, tint = .4)
navy30 <- col.tint(navy, tint = .3)
navy20 <- col.tint(navy, tint = .2)
navy10 <- col.tint(navy, tint = .1)

# black
black40 <- col.tint(black, tint = .4)
black30 <- col.tint(black, tint = .3)
black20 <- col.tint(black, tint = .2)
black10 <- col.tint(black, tint = .1)

# load libraries
library(extrafont)
library(ggplot2)
# if not run before, run
# font_import()

# define font sizes for normal text and headings in pt
font_size = 12
head_size = font_size + 2

# define theme for plot
ambition_theme <- theme_bw(base_family = "Segoe UI") + 
  theme(
    # font size and face settings
    text = element_text(family = "Segoe UI", size = font_size),
    title = element_text(face = "bold", size = font_size),
    plot.title = element_text(size = head_size, face = "bold"),
    plot.subtitle = element_text(size = font_size, face = "plain"),
    axis.title = element_text(size = font_size, face = "bold"),
    axis.text = element_text(size = font_size),
    legend.title = element_text(size = font_size, face = "bold"),
    legend.text = element_text(size = font_size),
    strip.text = element_text(size = font_size),
    plot.caption = element_text(size = font_size, face = "plain", hjust = 0), # any figure notes positioned left-bound
    # correct tint of grey for panels
    strip.background = element_rect(fill = black20),
    # legend positioned at bottom
    legend.position = "bottom"
  )

# create a very minimal theme was discussed with Andy to facilitate export
ambition_theme_minimal <- ambition_theme +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# function to convert units from pt to mm
# source: https://github.com/zmorrissey/commonR/blob/master/R/pt_to_mm.R

pt_to_mm <- function(x_pt) {
  ## Convert x from pt to mm (for, e.g., ggplot2 linewidths).
  return(x_pt / ggplot2::.pt)
}
