
source(here::here("./prep/prep.r"))
source(here::here("./scripts/00 cleaning script.R"))
source(here::here("./scripts/01 analysis script.R"))
source(here::here("./scripts/01 analysis subgraphs script.R"))


#dat6_non6 is list of all the organizations,

#matches_non is the edge list

#State boundaries of Nigeria
states <- rgeoboundaries::gb_adm1("Nigeria")

#identify the centroid for each state
# and rename the FCT to match the dataset
state_centers <- states |> 
  group_by(shapeName) |> 
  summarize(centers = st_centroid(geometry)) |> 
  mutate(shapeName = case_when(shapeName == "Abuja Federal Capital Territory" ~ "Federal Capital Territory"
                               , TRUE ~ shapeName))

#Join the nodes and the centers
dat6_sf <- dat6_non6 |> 
  left_join(state_centers, by = join_by("states_fixed2" == "shapeName")) |> 
  filter(!st_is_empty(centers)) |> 
  st_as_sf()



#jitter the points so they are not all on top of each other
dat6_sf1 <- dat6_sf |>
  dplyr::mutate(jitter_points = sf::st_jitter(centers, .1)) |> 
  st_set_geometry("jitter_points") |> 
  select(-centers) |> 
  rename(from = organization)

#Creating objects to shade the states by whether or not a cohort works there

dat6_states_work <- dat6_non6 |> 
  separate_rows(states_work, sep = ";") 

dat6_states_work$states_work <- dat6_states_work$states_work |> 
  str_trim()

dat6_states_work <- dat6_states_work |>  
  filter(!is.na(states_work) & 
           states_work != "" &
           states_work != "Don't know/Decline to answer" &
           states_work != "Decline to Answer") |> 
  count(cohort, states_work) |> 
  mutate(states_work = case_when(states_work == "Federal Capital Territory" ~ "Abuja Federal Capital Territory"
                                 , TRUE ~ states_work)) |> 
  mutate(popup_text = paste0(n, " ", cohort, " organizations work in ", states_work))

dat6_states_work_sf <- dat6_states_work |> 
  left_join(states, by = join_by("states_work" == "shapeName")) |> 
  st_as_sf()

bc_states_work_sf <- dat6_states_work_sf |> 
  filter(cohort == "Behavior Change")

jb_states_work_sf <- dat6_states_work_sf |> 
  filter(cohort == "JoinBodi")

mj_states_work_sf <- dat6_states_work_sf |> 
  filter(cohort == "Media and Journalism")

cj_states_work_sf <- dat6_states_work_sf |> 
  filter(cohort == "Criminal Justice")

#THis is for an overall count
dat6_states_work_all <- dat6_states_work |>  
  group_by(states_work) |> 
  summarize(total = sum(n)) |> 
  mutate(popup_text = paste0(total, " organizations work in ", states_work))

dat6_states_work_all_sf <- dat6_states_work_all |> 
  left_join(states, by = join_by("states_work" == "shapeName")) |> 
  st_as_sf()

#create an edges dataframe to use for this object
# that is based on the same one used elsewhere in the
#analysis
matches_non_geo <- matches_non |> 
  filter(from %in% dat6_sf1$from & to.x %in% dat6_sf1$from) |> 
  select(from, "to" = to.x)




#Confirm that dropped_rows contain points outside of Nigeria
# or for orgs that we don't know their hq location
#dropped_rows <- anti_join(matches_non, matches_non_geo, by = colnames(matches_non))

#make the sfnetwork object using the nodes and edges objects
# this also draws the lines
net <- sfnetworks::sfnetwork(nodes = dat6_sf1
                             , edges = matches_non_geo
                             , node_key = "organization"
                             , edges_as_lines = TRUE)

#edges are index numbers but I want names of orgs
# the next few lines add the org names to the edges dataset
nodes_df <- as_tibble(activate(net, "nodes")) # Extract nodes with names
edges_df <- as_tibble(activate(net, "edges")) 

#subset the node organizations for from and too based on 
#nodes_df and edges_df
net <- net |> 
  activate("edges") |> 
  mutate(from_name = nodes_df$from[from],
         to_name = nodes_df$from[to])

#Make subgraphs of each cohort

joinbodi <- "JoinBodi"
bechange <- "Behavior Change"
crimjust <- "Criminal Justice"
medjourn <- "Media and Journalism"
nongrantee <- "Non Grantee"

neighborhood_fun_geo <- function(x) {
  
  target_nodes <- V(net)[which(V(net)$cohort %in% x)]
  
  
  neighbors_list <- unique(unlist(
    igraph::neighborhood(net
                         , order = 1
                         , mode = "all"
                         , nodes = target_nodes
    )))
  
  
  filtered_nodes <- V(net)[c(as.integer(target_nodes)
                               , neighbors_list)]
  
  result <- igraph::induced_subgraph(net
                                     , vids = filtered_nodes) 
  
  result <- as_sfnetwork(result)
}

jb_subgraph <- neighborhood_fun_geo(x = joinbodi)
bc_subgraph <- neighborhood_fun_geo(x = bechange)
mj_subgraph <- neighborhood_fun_geo(x = medjourn)
cj_subgraph <- neighborhood_fun_geo(x = crimjust)
ng_subgraph <- neighborhood_fun_geo(x = nongrantee)

jb_subgraph <- jb_subgraph |>
  activate("nodes") |>
  filter(!is.na(cohort)) |>
  mutate(cohort = droplevels(factor(cohort))) |>
  sfnetworks::as_sfnetwork()


bc_subgraph <- bc_subgraph |> 
  activate("nodes") |>
  filter(!is.na(cohort)) |>
  mutate(cohort = droplevels(factor(cohort))) |>
  sfnetworks::as_sfnetwork()

mj_subgraph <- mj_subgraph |>
  activate("nodes") |>
  filter(!is.na(cohort)) |>
  mutate(cohort = droplevels(factor(cohort))) |>
  sfnetworks::as_sfnetwork()

cj_subgraph <- cj_subgraph |> 
  activate("nodes") |>
  filter(!is.na(cohort)) |>
  mutate(cohort = droplevels(factor(cohort))) |>
  sfnetworks::as_sfnetwork()

ng_subgraph <- ng_subgraph |>
  activate("nodes") |>
  filter(!is.na(cohort)) |>
  mutate(cohort = droplevels(factor(cohort))) |>
  sfnetworks::as_sfnetwork()


# making a golden color ramp
library(RColorBrewer)
library(colorspace)

# Define your base color
base_color <- "#B5984F"

# Method 1: Convert hex to HCL (using proper conversion)
# First convert hex to RGB, then to polarLUV (which is HCL)
base_rgb <- hex2RGB(base_color)
base_hcl <- as(base_rgb, "polarLUV")
# Now you can access H, C, L values
h_val <- base_hcl@coords[, "H"]
c_val <- base_hcl@coords[, "C"]
l_val <- base_hcl@coords[, "L"]

# Create sequential palette based on this color
my_palette1 <- sequential_hcl(7, h = h_val, c = c_val, l = c(30, 90))

# Method 2: Using lighter/darker versions directly
my_palette2 <- c(
  lighten(base_color, 0.7),
  lighten(base_color, 0.35),
  base_color,
  darken(base_color, 0.2),
  darken(base_color, 0.4)
)

#plotting the map
tm <- tmap::tmap_mode("view") +
  tmap::tm_shape(dat6_states_work_all_sf
           , name = "Overall") +
  tmap::tm_polygons(col = "total", fill_alpha = .9
              , popup.vars = "popup_text"
              , palette = carto_pal(6, "Burg")
              , na.show = FALSE
              , textNA = ""
              , legend.show = FALSE) +
  tmap::tm_shape(bc_states_work_sf
           , name = "Behavior Change states") +
  tmap::tm_polygons(col = "n", col_alpha = .9
              , popup.vars = "popup_text"
              , legend.show = FALSE
              , palette = "Blues") +
  tmap::tm_shape(jb_states_work_sf
        , name = "JoinBodi states") +
  tmap::tm_polygons(col = "n", alpha = .9
              , popup.vars = "popup_text"
              , legend.show = FALSE
              , palette = "Purples") +
  tmap::tm_shape(mj_states_work_sf
           , name = "Media and Journalism states") +
  tmap::tm_polygons(col = "n", alpha = .9
              , popup.vars = "popup_text"
              , legend.show = FALSE
              , palette = my_palette2) +
  tmap::tm_shape(cj_states_work_sf
           , name = "Criminal Justice states") +
  tmap::tm_polygons(col = "n", alpha = .9
              , popup.vars = "popup_text"
              , legend.show = FALSE
              , palette = "Oranges") +
  tmap::tm_shape(st_as_sf(net, "edges")
           , name = "All Connections") +
  tmap::tm_lines(col = "#636463", alpha = .4
           , lwd = 3
           , popup.vars = c("From: " = "from_name"
                            , "To: " = "to_name")) +
  tmap::tm_shape(st_as_sf(net, "nodes")
           , name = "All Organizations") +
  tmap::tm_dots(alpha = .6, shape = "desc_org", size = .5
          , popup.vars = c("Organization: " = "from"
                           )
          , title.shape = "Type of Organization") +
  tmap::tm_shape(st_as_sf(jb_subgraph, "edges")
           , name = "JoinBodi Connections") +
  tmap::tm_lines(col = "#5B507A", alpha = .4
           , lwd = 3
           , popup.vars = c("From: " = "from_name"
                            , "To: " = "to_name"))+
  tmap::tm_shape(st_as_sf(jb_subgraph, "nodes")
           , name = "JoinBodi Organizations"
           , group = "Cohort Layers") +
  tmap::tm_dots(col = "cohort", shape = "desc_org", size = .5, alpha = 1,
          , popup.vars = c("Organization: " = "from")
          , palette = palette
          , legend.shape.show = FALSE
          , title.col = "Cohort"
          , na.show = FALSE
          , textNA = "") +
  tmap::tm_shape(st_as_sf(bc_subgraph, "edges")
           , name = "Behavior Change Connections") +
  tmap::tm_lines(col = "#0964B0", alpha = .4
           , lwd = 3
           , popup.vars = c("From: " = "from_name"
                            , "To: " = "to_name")) +
  tmap::tm_shape(st_as_sf(bc_subgraph, "nodes")
           , name = "Behavior Change Organizations") +
  tmap::tm_dots(col = "cohort", alpha = 1, shape = "desc_org", size = .5
          , popup.vars = c("Organization: " = "from")
          , legend.show = FALSE
          , title.col = "Cohort"
          , na.show = FALSE
          , textNA = ""
          , palette = palette
          , legend.shape.show = FALSE) +
  tmap::tm_shape(st_as_sf(mj_subgraph, "edges")
             , name = "Media and Journalism Connections") +
  tmap::tm_lines(col = "#B5984F", alpha = .4
             , lwd = 3
             , popup.vars = c("From: " = "from_name"
                              , "To: " = "to_name")) +
  tmap::tm_shape(st_as_sf(mj_subgraph, "nodes")
           , name = "Media and Jouranlism Organizations") +
  tmap::tm_dots(col = "cohort", alpha = 1, shape = "desc_org", size = .5
            , popup.vars = c("Organization: " = "from")
            , legend.show = FALSE
            , palette = palette
            , legend.shape.show = FALSE) +
  tmap::tm_shape(st_as_sf(cj_subgraph, "edges")
             , name = "Criminal Justice Connections") +
  tmap::tm_lines(col = "#E17F48", alpha = .4
             , lwd = 3
             , popup.vars = c("From: " = "from_name"
                              , "To: " = "to_name")) +
  tmap::tm_shape(st_as_sf(cj_subgraph, "nodes")
           , name = "Criminal Justice Organizations") +
  tmap::tm_dots(col = "cohort", alpha = 1, shape = "desc_org", size = .5
            , popup.vars = c("Organization: " = "from")
            , legend.show = FALSE
            , palette = palette
            , legend.shape.show = FALSE) +
  tmap::tm_shape(st_as_sf(ng_subgraph, "edges")
          , name = "Non Grantee Connections") +
  tmap::tm_lines(col = "grey", alpha = .4
           , lwd = 3
           , popup.vars = c("From: " = "from_name"
                            , "To: " = "to_name")) +
  tmap::tm_shape(st_as_sf(ng_subgraph, "nodes")
           , name = "Non Grantee Organizations") +
  tmap::tm_dots(col = "cohort", alpha = 1, shape = "desc_org", size = .5
          , popup.vars = c("Organization: " = "from")
          , legend.show = FALSE
          , palette = palette
          , legend.shape.show = FALSE) + 
  tmap::tm_layout(title = "Interactive Map of the On Nigeria Anticorruption Network"
                  , legend.group = TRUE)

tm <- tm |> tmap_leaflet() |> 
  leaflet::hideGroup(c("JoinBodi Connections"
                       , "Behavior Change Connections"
                       , "Media and Journalism Connections"
                       , "Criminal Justice Connections"
                       , "Non Grantee Connections"
                       , "Behavior Change states"
                       , "JoinBodi states"
                       , "Media and Journalism states"
                       , "Criminal Justice states"
                       , "Cohort Layers"))


# animating a cohort map

# I'm hashtagging this animation out since I've saved
# the .gif image to disk. When this runs it causes some unintended
# output to auto-generate
#anim <- tmap::tmap_mode("plot") +
#  tmap::tm_shape(dat6_states_work_sf
#                 , name = "Overall") +
#  tmap::tm_polygons(fill = "n" 
#                    , palette = carto_pal(6, "Burg"), na.show = FALSE, textNA = ""
#                    , fill.free = FALSE
 #                   , title = "# of Orgs. working in State") +
#  tm_text("states_work", size = .8
#          , col = "black"
#          , bg.color = "white"
#          , bg.alpha = 1
#  ) +
#  tm_facets_pagewise(by = "cohort")
#
#tmap_animation(anim,
#               , fps = .5
#               , filename = "anim_map_cohorts.gif")
