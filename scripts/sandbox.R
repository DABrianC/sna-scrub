
# identifying nodes within 2 nodes of a selected node



node_fun <- function (node) {

  which(V(g_non)$name == {{node}})
  
}

connections_fun <- function (x) {
  ego(g_non
      , order = 2
      , nodes = x
      , mode = "all") |> 
    as.data.frame() 
    
}

node_fun("AD 4 Radio")

#AD 4 Radio = 64
AD4R <- ego(g_non, order = 2
            , nodes = 1
            , mode = "all") |> 
  as.data.frame()

node_fun("African Foundation for Young Media Professionals")

#AFYMP = 65
AFYMP <- connections_fun(2)

#Bureau of Public Service Reforms = 67
node_fun("Bureau of Public Service Reforms")

BPSR <- connections_fun(67)

node_fun("Ministry of Justice")

MOJ <- connections_fun(89)

#COMPPART = 73
node_fun("COMPPART Foundation for Justice and Peacebuilding")

comppart <- connections_fun(73)


dontknow <- dat4_non4 |> 
  filter(module == "Don't know/Decline to answer")

dontknow$from

#why is some data being dropped from the dataset

#creating my edgelist
all_orgs <- data.frame(unique(c(dat4_non4$from, dat4_non4$to))) 

edgelist_from_dat_non <- dat4_non4 |> 
  select(from) |> 
  unique()

edgelist_to_dat_non <- dat4_non4 |>
  select(to) |> 
  unique()

dat_all_test <- all_orgs_non |> 
  left_join(dat4_non4, by = join_by("organization" == "from")
            , relationship = "one-to-many") |> 
  left_join(dat4_non4, by = join_by("organization" == "to")
             , relationship = "many-to-many") |> 
  distinct(organization, .keep_all = TRUE)

dat_missing <- all_orgs_non |> 
  anti_join(dat_all_test)

dat_module_y <- dat_all_test |> 
  filter(is.na(module.y))

dat_module_x <- dat_all_test |> 
  filter(is.na(module.x))

dat_non_to <- dat5_non5 |> 
  mutate(to_in_from = to %in% all) |> 
  filter(to_in_from == FALSE) |> 
  select(to_in_from, to) |> 
  distinct(to)

dat_non_grantee <- dat4_non4 |> 
  filter(module == "Non Grantee") |> 
  select(from) |> 
  distinct()

filter(V(g_non)$cohort == "Criminal Justice")

test <- dat5_non5 |> 
  filter(grantee == "Criminal Justice")

unique(dat5_non5$grantee)

unique(dat5_non5$cohort)

dat5_non5_dontknow <- dat5_non5 |> 
  filter(cohort == "")

subtitle <- "<span style = 'color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'color:#5B507A;'>**JoinBodi Cohort**</span>, <span style = 'color:#000000;'>**Non-grantees**</span>" 

last_dontknow <- dat5_non5 |> 
  filter(cohort == "Don't know/Decline to answer")

bit_test <- dat4 |> 
  filter(from == "Behavioural Insights Team (BIT)")

# Visualizations


#convex hulls on a graph

tg <- as_tbl_graph(g_non) |> 
  mutate(
    joinbodi = name %in% V(joinbodi_nbhd)$name,
    bechange = name %in% V(bechange_nbhd)$name,
    crimjust = name %in% V(crimjust_nbhd)$name,
    medjourn = name %in% V(medjourn_nbhd)$name
  )

g_non_un <- graph_from_data_frame(matches_non
                               , directed = FALSE
                               , vertices = dat5_non5
)

g_non_simplified <- igraph::simplify(g_non_un)

bb <- layout_as_backbone(g_non_simplified)
E(g_non_simplified)$col <- F
E(g_non_simplified)$col[bb$backbone] <- T

V(g_non_un)$joinbodi <- if_else(V(g_non_un)$name %in% V(joinbodi_nbhd)$name, "JoinBodi", NA_character_)

ggraph(g_non_un, layout = "stress") +
  geom_edge_link0(width = .2) +
  geom_node_point(aes(fill = cohort), shape = 21, size = 3)+
  geom_mark_hull(aes(x, y, group = joinbodi, fill = joinbodi, label = joinbodi)
                 , concavity = 4,
                 expand = unit(2, "mm")
                 , alpha = .25
                 ) +
  theme_graph()

ggraph(tg, layout = "stress") +
  geom_edge_link(color = "gray70") +
  geom_node_point(aes(color = cohort), size = 4) +
  
  # Convex Hulls
  ggforce::geom_mark_hull(aes(x = x, y = y, group = joinbodi)
                          , fill = "blue", alpha = .1, color = "blue" )
  
  ggforce::geom_mark_hull(aes(x = x, y = y, fill = bechange)) +
  ggforce::geom_mark_hull(aes(x = x, y = y, fill = crimjust)) 
  
  ,
                          layer_data = filter(tg, joinbodi),
                 color = "blue", fill = "blue", alpha = 0.1) +
                 #label = "JoinBodi") +
  
  ggforce::geom_mark_hull(aes(x = x, y = y, group = bechange), 
                layer_data = filter(tg, bechange),
                 color = "green", fill = "green", alpha = 0.1) 
                 #label = "Behavior Change") +
  
  ggforce::geom_mark_hull(aes(x = x, y = y, group = crimjust), 
                 data = filter(tg, crimjust),
                 color = "orange", fill = "orange", alpha = 0.1, 
                 label = "Criminal Justice") +
  
  ggforce::geom_mark_hull(aes(x = x, y = y, group = medjourn), 
                 data = filter(tg, medjourn),
                 color = "purple", fill = "purple", alpha = 0.1, 
                 label = "Media and Journalism") +
  
  theme_minimal() +
  labs(title = "Overlap of Induced Subgraphs with Convex Hulls") +
  theme(legend.position = "bottom")


set.seed(665)

## create network with a group structure
g <- sample_islands(9, 40, 0.4, 15)
g <- igraph::simplify(g)
V(g)$grp <- as.character(rep(1:9, each = 40))

bb <- layout_as_backbone(g, keep = 0.4)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(g, #|
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]
) +
  geom_edge_link0(aes(col = col), width = 0.2) +
  geom_node_point(aes(fill = grp), shape = 21, size = 3) +
  ggforce::geom_mark_hull(
    aes(x, y, group = grp, fill = grp),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(legend.position = "none")






unique(V(g_non)$color)


same_orgs <- dat5_non5 |> 
  filter(organization == to)

dat4_test <- dat4 |> 
  mutate(to = case_when(from == "African Centre for Leadership, Strategy and Development (Centre LSD)"
                        & to == "African Centre for Leadership, Strategy and Development (Centre LSD)" ~ "Shehu Musa Yar’Adua Foundation"
                        , TRUE ~ to))



# cross community nodes
communities <- V(g_non)$cohort


V(g_non)$community <- V(g_non)$cohort 

V(g_non)$names_pretty <- str_wrap(V(g_non)$name, width = 20)
#V(g_non)$filtered <- V(g_non)$grantee != "Non Grantee"

#E(g_non)$cross_community <- V(g_non)[ends(g_non, E(g_non))[,1]]$community != V(g_non)[ends(g_non, E(g_non))[,2]]$community

# Define the community to **exclude**
excluded_community <- "Non Grantee"

# Identify edges that **connect to the excluded community**
E(g_non)$involves_excluded <- V(g_non)[ends(g_non, E(g_non))[,1]]$community == excluded_community |
  V(g_non)[ends(g_non, E(g_non))[,2]]$community == excluded_community

# Identify cross-community edges **that do not involve the excluded community**
E(g_non)$cross_community <- V(g_non)[ends(g_non, E(g_non))[,1]]$community != 
  V(g_non)[ends(g_non, E(g_non))[,2]]$community & 
  !E(g_non)$involves_excluded

#Identify nodes that are at the ends of cross-community edges
cross_community_nodes <- unique(as.vector(ends(g_non, E(g_non)[E(g_non)$cross_community])))

#Add a label column that only applies to cross-community nodes
V(g_non)$label <- ifelse(V(g_non)$name %in% cross_community_nodes, V(g_non)$name, NA)
# Mark nodes to be hidden (those with type "B")
#V(g_non)$filtered <- V(g_non)$cohort != "Non Grantee"  

acronyms_filtered <- sapply(V(g_non)$label, extract_or_generate_acronym)

# Ensure uniqueness by appending numbers to duplicates
make_unique <- function(acronyms_filtered) {
  seen <- list()
  for (i in seq_along(acronyms_filtered)) {
    original <- acronyms_filtered[i]
    new_acronym <- original
    count <- 1
    
    # While the acronym already exists, append a number
    while (new_acronym %in% seen) {
      count <- count + 1
      new_acronym <- paste0(original, count)
    }
    
    # Store unique acronym
    seen[[new_acronym]] <- TRUE
    acronyms_filtered[i] <- new_acronym
  }
  return(acronyms_filtered)
}

# Apply uniqueness function
V(g_non)$acronyms_filtered <- make_unique(acronyms_filtered)

subtitle <- "<span style = 'font-size:18pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:18pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:18pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:18pt; color:#5B507A;'>**JoinBodi Cohort**</span>"

ggraph(g_non, "stress") +
  geom_edge_bundle_force(aes(alpha = !involves_excluded), edge_color = "white"
                  , edge_linewidth = .4
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = cohort)) +
  
  #foreground
  geom_edge_bundle_force(aes(alpha = cross_community, color = cross_community), width = 1)+
                     #, edge_linewidth = ifelse(color = TRUE, 2, .5))
                        #, edge_linewidth = .4
                      #, alpha = .8
                      #  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                      #                  , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
   geom_node_point(aes(size = degree, shape = grantee, color = cohort)) +
   #geom_node_point(data = ggraph(g_non_delete, "stress"), aes(size = degree, shape = grantee, color = cohort)) +
   geom_node_text(aes(label = acronyms_filtered), size = 4, repel = TRUE
                  , max.overlaps = Inf, na.rm = TRUE
                  , nudge_y = .05
                  ) +
  #scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .4)) +
  scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "lightgrey"))+
   scale_size(range = c(2, 6)) +
   scale_color_manual(values = palette)+
  # scale_edge_color_manual(values = c("grey", "red")) +
   labs(title = "Cross Community Connections "
        , subtitle = subtitle)+
   theme_graph() +
   theme(legend.position = "bottom"
         , plot.subtitle = element_markdown()) +
   guides(color = "none", fill = "none") +
  coord_cartesian(xlim = c(-2.5, 4), ylim = c(-4.5, 3))
 
                        
                

#acronyms

library(igraph)
library(stringr)  # For regex functions

# Create a sample graph with names
# Create a sample graph with names
g <- make_ring(7)
V(g)$name <- c("National Aeronautics and Space Administration (NASA)", 
               "Federal Bureau of Investigation (FBI)", 
               "Central Intelligence Agency", 
               "Environmental Protection Agency", 
               "Department of Homeland Security",
               "Google",  # Single-word name (should remain as is)
               "Tesla Inc")  # Two-word name (should generate an acronym)

# Function to extract or generate acronyms (only for multi-word names)
extract_or_generate_acronym <- function(name) {
  # Try to extract existing acronym from parentheses
  acronym <- str_extract(name, "\\(([^)]+)\\)")  # Extract text inside parentheses
  
  # If an acronym exists, remove parentheses and return
  if (!is.na(acronym)) {
    return(str_replace_all(acronym, "[()]", ""))  # Remove parentheses
  }
  
  # Split name into words
  words <- unlist(str_split(name, "\\s+"))
  
  # If name has only one word, return it as is
  if (length(words) == 1) {
    return(words)  
  }
  
  # Otherwise, generate acronym from capital letters
  generated_acronym <- str_extract_all(name, "[A-Z]") %>% unlist() %>% paste0(collapse = "")
  
  return(generated_acronym)
}

# Generate acronyms for all nodes
acronyms <- sapply(V(g_non)$name, extract_or_generate_acronym)

# Ensure uniqueness by appending numbers to duplicates
make_unique <- function(acronyms) {
  seen <- list()
  for (i in seq_along(acronyms)) {
    original <- acronyms[i]
    new_acronym <- original
    count <- 1
    
    # While the acronym already exists, append a number
    while (new_acronym %in% seen) {
      count <- count + 1
      new_acronym <- paste0(original, count)
    }
    
    # Store unique acronym
    seen[[new_acronym]] <- TRUE
    acronyms[i] <- new_acronym
  }
  return(acronyms)
}

# Apply uniqueness function
V(g_non)$acronym <- make_unique(acronyms)

# Print results
data.frame(Name = V(g_non)$name, Acronym = V(g_non)$acronym)

#### testing out clustering
set.seed(665)

## create network with a group structure
g <- sample_islands(9, 40, 0.4, 15)
g <- igraph::simplify(g)
V(g)$grp <- as.character(rep(1:9, each = 40))

bb <- layout_as_backbone(g, keep = 0.4)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(g, #|
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]
) +
  geom_edge_link2(aes(edge_colour = node.position), width = 0.2) +
  geom_node_point(aes(fill = grp), shape = 21, size = 3) +
  ggforce::geom_mark_hull(
    aes(x, y, group = grp, fill = grp),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(legend.position = "none")

#try with my data

g_non_undirected <- graph_from_data_frame(matches_non
                               , directed = FALSE
                               , vertices = dat5_non5
)


g_non_simple <- igraph::simplify(g_non_undirected
                         , remove.multiple = T)
V(g_non_simple)$grp <- V(g_non_simple)$cohort
bb <- layout_as_backbone(g_non_simple, keep = 0.4)
E(g_non_simple)$col <- F
E(g_non_simple)$col[bb$backbone] <- T

ggraph(g_non, #|
       layout = "stress"#,
       #, keep = .05
       #, x = bb$xy[, 1],
       #y = bb$xy[, 2]
) +
  geom_edge_bundle_force(color = "grey", alpha = .6, width = 0.2) +
  geom_node_point(aes(fill = cohort), shape = 21, size = 3) +
  ggforce::geom_mark_hull(
    aes(x, y, group = cohort
        , label = cohort
        , filter = cohort == "JoinBodi")
    , fill = palette[[3]]
    , label.colour = palette[[3]]
    , label.fill = NA
    , con.colour = palette[[3]],
    , concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  ggforce::geom_mark_hull(
    aes(x, y, group = cohort
        , label = cohort
        , filter = cohort == "Media and Journalism")
    , fill = palette[[4]]
    , label.colour = palette[[4]]
    , con.colour = palette[[4]],
    , concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) + 
  ggforce::geom_mark_hull(
    aes(x, y, group = cohort
        , label = cohort
        , filter = cohort == "Behavior Change")
    , fill = palette[[1]]
    , label.colour = palette[[1]]
    , con.colour = palette[[1]],
    , concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
    ) +
  ggforce::geom_mark_hull(
    aes(x, y, group = cohort
        , label = cohort
        , filter = cohort == "Criminal Justice")
    , fill = palette[[2]]
    , label.colour = palette[[2]]
    , con.colour = palette[[2]],
    , concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  #scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(legend.position = "none")


library(igraph)
library(ggraph)
library(dplyr)
library(scales)

# Create a sample graph
set.seed(42)
g <- make_ring(10) %>% 
  set_vertex_attr("group", value = rep(1:2, each = 5)) # Two groups

# Define node colors
node_colors <- c("red", "blue")
V(g)$color <- node_colors[V(g)$group]

# Convert to a tidygraph object
library(tidygraph)
g_tidy <- as_tbl_graph(g)

# Prepare edge colors by merging node colors
edges <- g_tidy %>%
  activate(edges) %>%
  mutate(edge_color = map2_chr(.N()$color[from], .N()$color[to], 
                               ~ scales::gradient_n_pal(c(.x, .y))(0.5)))

# Plot with ggraph
ggraph(edges) +
  geom_edge_link(aes(color = edge_color), edge_alpha = 0.7, edge_width = 1) +
  geom_node_point(aes(color = color), size = 5) +
  scale_edge_colour_identity() +  # Preserve precomputed colors
  scale_color_identity() +
  theme_graph()

library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)
library(scales)

library(igraph)
library(ggraph)
library(dplyr)
library(scales)

# Create a sample graph with 10 nodes
set.seed(42)
g <- make_ring(10)
V(g)$group <- rep(1:2, each = 5)  # Assign two groups
node_colors <- c("blue", "red")
V(g)$color <- node_colors[V(g)$group]  # Assign node colors based on groups

# Extract edge list and define attributes
edges_df <- as_data_frame(g, what = "edges") %>%
  mutate(
    from_color = V(g)$color[from],  
    to_color = V(g)$color[to],
    edge_type = ifelse(from_color == to_color, "same", "diff"),
    edge_id = row_number()  # Unique edge ID for interpolation
  )

# Convert back to igraph object with attributes
E(g)$from_color <- V(g)$color[from]
E(g)$to_color <- edges_df$to_color
E(g)$edge_type <- edges_df$edge_type
E(g)$edge_id <- edges_df$edge_id

# Plot with ggraph (keeping it as an igraph object)
ggraph(g, layout = "fr") + 
  # Gradient edges (cross-group)
  geom_edge_link2(
    data = edges_df %>% filter(edge_type == "diff"), 
    aes(color = ..index.., group = edge_id), edge_width = 1.2, edge_alpha = 0.8
  ) +
  scale_edge_color_gradientn(colors = c("blue", "red")) +  # Smooth transition
  
  # Solid edges (same-group)
  geom_edge_link(
    data = edges_df %>% filter(edge_type == "same"),
    aes(color = node.position), edge_width = 1, edge_alpha = 0.7
  ) +
  scale_color_identity() +  # Ensure node colors are preserved
  theme_graph() + 
  geom_node_point(aes(color = color), size = 6)


data(vaccinations)
vaccinations <- transform(vaccinations,
                          response = factor(response, rev(levels(response))))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")

### Sankey plots of flows across cohorts

df_cohorts <- dat6_non6 |> 
  select(organization, cohort) 

df_sankey <- matches_non |> 
  left_join(df_cohorts, by = join_by(from == organization)) |> 
  left_join(df_cohorts, by = join_by(to.x == organization)) |> 
  group_by(cohort.x, cohort.y) |> 
  count(cohort.x, cohort.y) |> 
  group_by(cohort.x) |> 
  arrange(desc(cohort.x)) |> 
  ungroup() |> 
  mutate(total = cumsum(n)) |> 
  group_by(cohort.x) |> 
  mutate(percent = n/sum(n)*100
         , midpoint = cumsum(n) - n/2) 
  #ungroup() |> 
  #mutate(midpoint = cumsum(n) - n/2)
  
#Overall sankey chart
ggplot(df_sankey, aes(axis1 = cohort.x
                      , axis2 = cohort.y
                      , fill = cohort.x
                      , y = n)) +
  geom_flow() +
  #geom_stratum(aes(fill = cohort.x))+
  geom_stratum(aes(fill = after_stat(stratum))
               , color = "white"
               , width = .4)+
  geom_text(stat = "stratum", 
            aes(label = str_wrap(after_stat(stratum)
                                 , width = 14))
            , color = "white") +
  #geom_text(aes(label = paste0(round(percent, digits = 0), "%")
    #            , y = total-2)
     #       , x = 1.75) +            
  scale_x_discrete(limits = c("Behavior Change"
                              , "Criminal Justice"
                              , "JoinBodi"
                              , "Media and Journalism"
                              , "Non Grantee")
                   , expand = c(.1, .1)) +
  #scale_x_discrete(limits = c("cohort.x", "cohort.y")) +
  theme_void() +
  theme(legend.position = "none"
  )+
  labs(title = "Most collaborations are among cohorts and non-grantees"
       , x = "From cohort"
       , y = "To cohort") +
  scale_fill_manual(values = c("Behavior Change" = palette[1]
                               , "Criminal Justice" = palette[2]
                               , "JoinBodi" = palette[3]
                               , "Media and Journalism" = palette[4]
                               , "Non Grantee" = palette[5]
                               , "Other" = "lightgrey"))

highlight_category <- "Behavior Change"

df_sankey <- df_sankey |> 
  mutate(highlight = ifelse(cohort.x == "Behavior Change", cohort.x, "Other"))


ggplot(df_sankey, aes(axis1 = cohort.x
                      , axis2 = cohort.y
                      , y = n)) +
  geom_alluvium(aes(fill = highlight, alpha = ifelse(highlight == "Other", .4, 1))
                , show.legend = FALSE) +
  #geom_stratum(aes(fill = cohort.x))+
  geom_stratum(aes(fill = after_stat(stratum))
               , color = "white"
               , width = .4
  )+
  #geom_text(stat = "stratum", 
    #        aes(label = str_wrap(after_stat(stratum)
    #                             , width = 16))
    #        , color = "white") +
  #geom_text(aes(label = ifelse(highlight == "Behavior Change"
            #                   , paste0(round(percent), "%"), "")
             #   , y = total -5)
             #   , x = 1.75
             #   , size = 4)+
  scale_x_discrete(limits = c("Behavior Change"
                              , "Criminal Justice"
                              , "JoinBodi"
                              , "Media and Journalism"
                              , "Non Grantee")
                   , , expand = c(.1, .1)) +
  #scale_x_discrete(limits = c("cohort.x", "cohort.y")) +
  theme_void() +
  theme(legend.position = "none"
  )+
  labs(title = "Behavior Change"
       , x = "From cohort"
       , y = "To cohort") +
  scale_fill_manual(values = c("Behavior Change" = palette[1]
                               , "Criminal Justice" = palette[2]
                               , "JoinBodi" = palette[3]
                               , "Media and Journalism" = palette[4]
                               , "Non Grantee" = palette[5]
                               , "Other" = "lightgrey"))


#define highlight category
sankey_fun <- function(x) {

highlight_category <- x

df_sankey <- df_sankey |> 
  mutate(highlight = ifelse(cohort.x == highlight_category, cohort.x, "Other")) |> 
  arrange(highlight == "Other")
         #, cohort.x = str_wrap(cohort.x, width = 12)) 

ggplot(df_sankey, aes(axis1 = cohort.x
                      , axis2 = cohort.y
                      , y = n)) +
  geom_alluvium(aes(fill = highlight, alpha = ifelse(highlight == "Other", .4, 1))
                , show.legend = FALSE) +
  #geom_stratum(aes(fill = cohort.x))+
  geom_stratum(aes(fill = after_stat(stratum))
               , color = "white"
               , width = .4
               )+
  geom_text(stat = "stratum", 
            aes(label = str_wrap(after_stat(stratum)
                                 , width = 16))
            , color = "white") +
  scale_x_discrete(limits = c("Behavior Change"
                              , "Criminal Justice"
                              , "JoinBodi"
                              , "Media and Journalism"
                              , "Non Grantee")
                   , , expand = c(.1, .1)) +
  #scale_x_discrete(limits = c("cohort.x", "cohort.y")) +
  theme_void() +
  theme(legend.position = "none"
        )+
  labs(title = x
       , x = "From cohort"
       , y = "To cohort") +
  scale_fill_manual(values = c("Behavior Change" = palette[1]
                               , "Criminal Justice" = palette[2]
                               , "JoinBodi" = palette[3]
                               , "Media and Journalism" = palette[4]
                               , "Non Grantee" = palette[5]
                               , "Other" = "lightgrey"))
                      
}

bc_sankey <- sankey_fun(x = "Behavior Change")
cj_sankey <- sankey_fun(x = "Criminal Justice")
jb_sankey <- sankey_fun(x = "JoinBodi") 
mj_sankey <- sankey_fun(x = "Media and Journalism")
ng_sankey <- sankey_fun(x = "Non Grantee")


library(ggraph)
library(igraph)
library(tidygraph)
library(dplyr)
library(tidyr)

# Create an example graph with node colors
graph <- tbl_graph(
  nodes = tibble(name = c("A", "B", "C"), color = c("red", "blue", "green")),
  edges = tibble(from = c(1, 2), to = c(2, 3))
)

# Generate node positions using the "fr" (Fruchterman-Reingold) layout
layout <- create_layout(graph, layout = "fr")

# Add x, y positions to nodes
node_positions <- layout %>% select(name, x, y)

# Merge node positions back into the graph's edges
graph_edges <- graph %>%
  activate(edges) %>%
  as_tibble() %>%
  left_join(node_positions, by = c("from" = "name")) %>%
  rename(x_from = x, y_from = y) %>%
  left_join(node_positions, by = c("to" = "name")) %>%
  rename(x_to = x, y_to = y)

# Create two segments per edge (before & after midpoint)
edge_segments <- graph_edges %>%
  mutate(
    midpoint_x = (x_from + x_to) / 2,
    midpoint_y = (y_from + y_to) / 2
  ) %>%
  pivot_longer(cols = c(from, to), names_to = "segment", values_to = "node") %>%
  mutate(
    edge_color = ifelse(segment == "from", .N()$color[from], .N()$color[to]),
    start_x = ifelse(segment == "from", x_from, midpoint_x),
    start_y = ifelse(segment == "from", y_from, midpoint_y),
    end_x = ifelse(segment == "from", midpoint_x, x_to),
    end_y = ifelse(segment == "from", midpoint_y, y_to)
  )

# Convert modified edges to an igraph object
edge_graph <- graph_from_data_frame(edge_segments, directed = FALSE, vertices = node_positions)

# Plot with hard color switch at the midpoint
ggraph(edge_graph, layout = "manual", node.position = node_positions) +
  geom_edge_link0(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, color = edge_color), edge_width = 2) +
  geom_node_point(aes(color = color), size = 6) +
  scale_color_manual(values = c("red", "blue", "green")) +  
  theme_void()

data(greys, package = "networkdata")


ggraph(greys, "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = node.position), edge_linewidth = 0.5) +
  geom_node_point(aes(fill = sex), shape = 21, size = 3) +
  #geom_node_text(aes(label = name, size = degree(greys)),
  #               family = "serif", repel = TRUE
  #) +
  scale_edge_colour_brewer(palette = "Set1") +
  scale_fill_manual(values = c("grey66", "#EEB422", "#424242")) +
  scale_size(range = c(2, 5), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")


greys_tbl <- as_tbl_graph(greys)

## Trying to make the edges change color
ggraph(g_non, "stress") +
  geom_edge_link0(aes(alpha = !involves_excluded), edge_colour = "white"
                         , edge_linewidth = .4
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed")
                  , show.legend = FALSE) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = cohort)) +
  
  #foreground
  geom_edge_link0(aes(alpha = cross_community
                      , edge_colour = cross_community), edge_linewidth = 1
                  , show.legend = FALSE)+
  geom_node_point(aes(size = degree
                      , shape = grantee, color = cohort)) +
  #geom_node_point(data = ggraph(g_non_delete, "stress"), aes(size = degree, shape = grantee, color = cohort)) +
  geom_node_text(aes(label = acronyms_filtered), size = 4, repel = TRUE
                 , max.overlaps = Inf, na.rm = TRUE
                 , nudge_y = .05
  ) +
  #scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
  scale_edge_color_manual(values = c("TRUE" = "red", "FALSE" = "white"))+
  scale_size(range = c(2, 6)) +
  scale_color_manual(values = palette)+
  # scale_edge_color_manual(values = c("grey", "red")) +
  labs(title = "Cross Cohort Connections "
       , subtitle = subtitle)+
  theme_graph() +
  theme(legend.position = "bottom"
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")

library(ggraph)
library(igraph)
library(tidyverse)

# Create a simple graph
set.seed(123)
graph <- make_ring(10)

# Assign node names explicitly
V(graph)$name <- as.character(seq_len(vcount(graph))) 

# Assign colors to nodes
V(graph)$color <- RColorBrewer::brewer.pal(10, "Set3")

# Convert graph to edge data frame
edge_df <- get.data.frame(graph, what = "edges")

# Convert graph to node data frame with explicit names
node_df <- data.frame(name = V(graph)$name, color = V(graph)$color, stringsAsFactors = FALSE)

# Merge edge data with node colors
edge_df <- edge_df %>%
  left_join(node_df, by = c("from" = "name")) %>%
  rename(source_color = color) %>%
  left_join(node_df, by = c("to" = "name")) %>%
  rename(target_color = color)

# Function to interpolate edge colors
interpolate_color <- function(start_color, end_color, n = 100) {
  scales::colour_ramp(c(start_color, end_color))(seq(0, 1, length.out = n))
}

# Apply interpolation to each edge
edge_df <- edge_df %>%
  rowwise() %>%
  mutate(edge_color = list(interpolate_color(source_color, target_color, n = 10))) %>%
  ungroup()

# Flatten color list for ggraph
edge_df <- edge_df %>%
  mutate(edge_color = map_chr(edge_color, ~ .[5]))  # Pick middle transition color

# Plot graph
ggraph(graph, layout = "circle")


glimpse(dat5_non5)

length(dat5_non5$desc_org[dat5_non5$desc_org=="Private Business/Company"])


paste0(round(length(dat5_non5$desc_org[dat5_non5$desc_org=="Private Business"])/(nrow(dat5_non5))*100, 0), " percent")

paste0(round(length(dat5_non5$desc_org[dat5_non5$desc_org=="Civil Society Organization (CSO)"])/(nrow(dat5_non5))*100, 0), " percent")

paste0(round(length(dat5_non5$desc_org[dat5_non5$desc_org=="MDA"])/(nrow(dat5_non5))*100, 0), " percent")

paste0(round(length(dat5_non5$desc_org[dat5_non5$desc_org=="NGO"])/(nrow(dat5_non5))*100, 0), " percent")

paste0(round(length(dat5_non5$desc_org[dat5_non5$desc_org=="INGO"])/(nrow(dat5_non5))*100, 0), " percent")


library(igraph)

# Create example graph
set.seed(42)
g <- erdos.renyi.game(15, p = 0.3, directed = FALSE)

# Calculate node degrees
degree_values <- igraph::degree(g, mode = "all")

# Compute degree threshold (top 25% most connected nodes)
degree_threshold <- quantile(degree_values, 0.75, na.rm = TRUE)

# Identify hub nodes
hub_nodes <- V(g)[degree_values >= degree_threshold]

# Set node colors: Red for hubs, light gray for others
V(g)$color <- ifelse(V(g) %in% hub_nodes, "red", "lightgray")

# Set node sizes: Bigger for hubs
V(g)$size <- ifelse(V(g) %in% hub_nodes, 15, 7)

# Plot graph
plot(g, vertex.label = NA, main = "Hub Nodes Highlighted")

#centrality plot by betweenness
subtitle <- "<span style = 'font-size:22pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:22pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:22pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:22pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 
ggraph(g_non, layout = "centrality", cent = igraph::betweenness(g_non))+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                       , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                       , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::betweenness(g_non), shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::betweenness(g_non), shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::betweenness(g_non) >= 1000, label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")

#centrality plot by eigenvector
subtitle <- "<span style = 'font-size:22pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:22pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:22pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:22pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 
ggraph(g_non, layout = "centrality", cent = igraph::eigen_centrality(g_non)$vector)+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::eigen_centrality(g_non)$vector, shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::eigen_centrality(g_non)$vector, shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::eigen_centrality(g_non)$vector >= .29, label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")

#centrality by degree
subtitle <- "<span style = 'font-size:22pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:22pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:22pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:22pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 
ggraph(g_non, layout = "centrality", cent = igraph::strength(g_non))+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::strength(g_non), shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::strength(g_non), shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::strength(g_non) >= 14, label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")


#centrality by closeness
subtitle <- "<span style = 'font-size:22pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:22pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:22pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:22pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 
ggraph(g_non, layout = "centrality", cent = igraph::closeness(g_non, mode = "all"))+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::closeness(g_non, mode = "all"), shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::closeness(g_non, mode = "all"), shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::closeness(g_non, mode = "all") >= 1, label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")

#centrality by harmonic centrality
subtitle <- "<span style = 'font-size:22pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:22pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:22pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:22pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 
ggraph(g_non, layout = "centrality", cent = igraph::harmonic_centrality(g_non))+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::harmonic_centrality(g_non), shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::harmonic_centrality(g_non), shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::harmonic_centrality(g_non) >= 35, label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")


#centrality by transitivity
subtitle <- "<span style = 'font-size:18pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:18pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:18pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:18pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 
ggraph(g_non, layout = "centrality", cent = igraph::transitivity(g_non))+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::transitivity(g_non), shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::transitivity(g_non), shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::transitivity(g_non) >= quantile(igraph::transitivity(g_non), .9), label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(2, 10)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , legend.title = element_text(size = 20)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")




#density_df <- data.frame(network = c("Behavior Change", "Criminal Justice", "JoinBodi", "Media and Journalism", "Overall")
                          #                        , density = c(bechange_nbhd_density, crimjust_nbhd_density, joinbodi_nbhd_density, medjourn_nbhd_density, g_non_density)) |> 
                         #  mutate(density = round(density*100, digits = 0))
                         
                         ggplot(density_df)+
                           geom_col(aes(x = density, y = fct_reorder(network, density))
                                    , fill = "#00457C") +
                           geom_text(aes(x = density, y = fct_reorder(network, density)
                                         , label = paste0(density, "%"))
                                     , hjust = 1.25, color = "white"
                                     , fontface = "bold")+
                           theme_void() +
                           theme(axis.text.y = element_text(color = "black", size = 14, hjust = 1)
                                 , axis.title.x = element_text(size = 16, face = "bold")
                                 , plot.title = element_text(size = 18, face = "bold")
                                 , plot.title.position = "plot") +
                           labs(x = "Density"
                                , title = "Figure X. Network density for the overall network \nand individual cohort-level networks.") 
                         

density_df <- data.frame(network = c("Behavior Change", "Criminal Justice", "JoinBodi", "Media and Journalism", "Overall")
                                     , density = c(bechange_nbhd_density, crimjust_nbhd_density, joinbodi_nbhd_density, medjourn_nbhd_density, g_non_density)) |> 
                                mutate(density = round(density*100, digits = 0))

# Gesi-focused connections
# all gesi nodes and their connections

V(g_non)$flag <- V(g_non)$women_led=="Yes"| V(g_non)$gesi_focus=="Yes" | V(g_non)$gen_focus_serves_disadv_pop=="Yes"

flagged_nodes <- V(g_non)[!is.na(V(g_non)$flag)]
g_sub <- induced_subgraph(g_non, vids = flagged_nodes)
  

layout <- create_layout(g_non, layout = "stress")  

sub_nodes <- V(g_sub)$name

# Now plot g_non with highlight on g_sub
ggraph(layout) +
  # Edges (background graph)
  geom_edge_link(color = "gray80", edge_width = .8) +
  
  # Edges in the subgraph
  #geom_edge_link(
  #  data = get_edges(g_sub, layout),
  #  color = "purple", width = 1.5
  #) +
  
  # All nodes (background)
  geom_node_point(color = "gray60", size = 4) +
  
  # Highlighted nodes (subset)
  geom_node_point(
    data = filter(layout, name %in% sub_names),
    color = "purple", size = 6
  ) +
  
  geom_node_text(
    data = filter(layout, name %in% sub_nodes),
    aes(label = name),
    size = 3, repel = TRUE
  ) +
  
  theme_void()

ggraph(g_non, layout = "stress") +
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .8, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed")) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = flag)) +
  #geom_node_text(aes(filter = degree >= 9, label = names_pretty), size = 4, repel = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_color_manual(values = palette)+
  labs(title = "The Endline On Nigeria Accountability Network"
       , subtitle = subtitle
       , shape = ""
       , size = "Degree")+
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , legend.title = element_text(size = 16)
        , plot.subtitle = element_markdown()
        , plot.title = element_text(size = 20)) +
  guides(color = "none", fill = "none"
         , shape = guide_legend(override.aes = list(size = 6)))

ggraph(g_non, layout = "stress") +
  geom_edge_bundle_force(color = "gray80") +
  geom_node_point()
ggraph(g_sub, layout = "stress") +
  geom_edge_link(color = "gray80") +
  geom_node_point(aes(color = flag), size = 6) +
  geom_node_text(aes(label = acronym), repel = TRUE, size = 4) +
  scale_color_manual(values = c("gray", "purple")) +
  theme_void()
