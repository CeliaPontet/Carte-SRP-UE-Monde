library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(colourpicker)
library(RColorBrewer)
library(sp)

library(png)

library(rnaturalearth)
library(grid)

library(tidyverse)

data_ue <- read_rds("DATA/data_ue.rds")

ref <- read_excel("DATA/ref.xlsx", sheet="ref") %>% 
  mutate(PAYS = case_when(
    is.na(PAYS) ~ name,
    TRUE ~ PAYS
  ))

ti <- readPNG("./DATA/LogoTI.png")
tu <- readPNG("./DATA/LogoTU.png")




######################
# Couleurs
#####################

couleurs <- c("gold2","darkolivegreen3","coral2","mediumpurple4","steelblue3","deeppink3","darkgrey","aquamarine3","darkseagreen4","violetred4","lightpink2","lightblue2","yellow3")


#PalettePicker
# List of palettes
colors_pal <- lapply(
  X = split(
    x = brewer.pal.info,
    f = factor(brewer.pal.info$category, labels = c("Diverging", "Qualitative", "Sequential"))
  ),
  FUN = rownames
)

# Get all colors given a palette name(s)
get_brewer_name <- function(name) {
  pals <- brewer.pal.info[rownames(brewer.pal.info) %in% name, ]
  res <- lapply(
    X = seq_len(nrow(pals)),
    FUN = function(i) {
      brewer.pal(n = pals$maxcolors[i], name = rownames(pals)[i])
    }
  )
  unlist(res)
}

background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_name)
head(background_pals, 3)

# Calc linear gradient for CSS
linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))

colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))

