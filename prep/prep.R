# Set CRAN mirror to the cloud version of CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

 
packages <- c("bookdown", "concaveman", "cowplot", "extrafont", "extrafontdb"
              , "flextable", "ggalluvial", "ggiraph", "ggnetwork", "ggraph"
              , "ggtext", "gifski", "graphlayouts", "htmltools", "leaflet", "lubridate", "igraph", "janitor"
              ,"here", "knitr", "mapboxapi", "networkD3"
              , "patchwork", "quarto", "randomNames", "rcartocolor", "RColorBrewer", "reactable", "reactablefmtr", "readxl", "rgeoboundaries"
              , "scales", "sf", "sfnetworks", "sna", "threejs", "tidygraph", "tidyverse", "tmap"
              , "tmaptools", "viridis",  "visNetwork")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE) |>
  invisible()


palette <- c("#0964B0" #Behavior Change
             , "#E17F48" #Criminal Justice
             , "#5B507A" #JoinBodi
             , "#B5984F" #Media and Journalism
             , "grey")   #non-grantee

set.seed(145)


