
source(here::here("./prep/prep.r"))
source(here::here("./scripts/00 cleaning script.R"))
source(here::here("./scripts/01 analysis script.R"))

#cohort densities without any external connections
joinbodi <- "JoinBodi"
bechange <- "Behavior Change"
crimjust <- "Criminal Justice"
medjourn <- "Media and Journalism"

cohort_fun <- function(x) {
  
  target_nodes <- V(g_non)[which(V(g_non)$cohort %in% x)]
  
  result <- igraph::induced_subgraph(g_non
                                     , vids = target_nodes)
  
  igraph::edge_density(result)
}

jb_cohort_density <- cohort_fun(x = joinbodi)
bc_cohort_density <- cohort_fun(x = bechange)
cj_cohort_density <- cohort_fun(x = crimjust)
mj_cohort_density <- cohort_fun(x = medjourn)

#This only includes cohorts
density_df_cohorts <- data.frame(
  network = c("Behavior Change", "Criminal Justice", "JoinBodi", "Media and Journalism", "Overall")
  , density = c(bc_cohort_density, cj_cohort_density, jb_cohort_density, mj_cohort_density, g_non_density)) |> 
  mutate(density = round(density*100, digits = 0))

## Induced subgraphs of neighborhoods
#These include the cohort and the
# first order connections
joinbodi <- "JoinBodi"
bechange <- "Behavior Change"
crimjust <- "Criminal Justice"
medjourn <- "Media and Journalism"

neighborhood_fun <- function(x) {

  target_nodes <- V(g_non)[which(V(g_non)$cohort %in% x)]
 
  neighbors_list <- unique(unlist(
    igraph::neighborhood(g_non
                         , order = 1
                         , mode = "all"
                         , nodes = target_nodes
    )))

 
  filtered_nodes <- V(g_non)[c(as.integer(target_nodes)
                               , neighbors_list)]
  
  result <- igraph::induced_subgraph(g_non
                                     , vids = filtered_nodes)

  }

#create objects for each of the four neighborhoods
# each neighborhood is all of the orgs from each cohort
# plus all the first order connections
joinbodi_nbhd <- neighborhood_fun(x = joinbodi)
bechange_nbhd <- neighborhood_fun(x = bechange)
crimjust_nbhd <- neighborhood_fun(x = crimjust)
medjourn_nbhd <- neighborhood_fun(x = medjourn)

#Calculate centrality stats for each neighborhood

centrality_funs <- function(graph) {
  graph_name <- deparse(substitute(graph))
  
  degree_end <- igraph::degree(graph) |> 
    as.data.frame()
  
  degree_end_out <- igraph::degree(graph, mode = "out") |> 
    as.data.frame()
  
  degree_end_in <- igraph::degree(graph, mode = "in") |> 
    as.data.frame()
  
  strength_end <- igraph::strength(graph) |> 
    as.data.frame()
  
  density_end <- igraph::edge_density(graph) |> 
    as.data.frame()
  
  between_end <- igraph::betweenness(graph) |> 
    as.data.frame()
  
  eigen_central_end <- igraph::eigen_centrality(graph)$vector |> 
    as.data.frame()
  
  transitivity_end <- igraph::transitivity(graph) |> 
    as.data.frame()
  
  harmonic_end <- igraph::harmonic_centrality(graph) |> 
    as.data.frame()
  
  closeness_end <- igraph::closeness(graph, mode = "all") |> 
    as.data.frame()

  centrality_end <- bind_cols(between_end, harmonic_end 
                              , eigen_central_end, degree_end
                              , degree_end_out, degree_end_in) |>   
    rownames_to_column(var = "Organization") |> 
    rename("Betweenness" = "igraph::betweenness(graph)"
           , "Harmonic Centrality" = "igraph::harmonic_centrality(graph)"
           , "Eigen" = "igraph::eigen_centrality(graph)$vector"
           , "Degree" = "igraph::degree(graph)"
           , "Out-degree" = "igraph::degree(graph, mode = \"out\")"
           , "In-degree" = "igraph::degree(graph, mode = \"in\")")
  
  
  centrality_end <- centrality_end |> 
    group_by(Organization) |> 
    summarize(
      across(.cols = 1:6
             , .fns = ~round(.x, digits = 2))
    )

  assign(paste0(graph_name, "_degree"), degree_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_degree_out"), degree_end_out, envir = .GlobalEnv)
  assign(paste0(graph_name, "_degree_in"), degree_end_in, envir = .GlobalEnv)
  assign(paste0(graph_name, "_strength"), strength_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_density"), density_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_betweenness"), between_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_eigen"), eigen_central_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_transitivity"), transitivity_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_harmonic"), harmonic_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_closeness"), closeness_end, envir = .GlobalEnv)
  assign(paste0(graph_name, "_centrality_scores"), centrality_end, envir = .GlobalEnv)
}



centrality_funs(joinbodi_nbhd)
centrality_funs(crimjust_nbhd)
centrality_funs(bechange_nbhd)
centrality_funs(medjourn_nbhd)

# top 10 organizations by statistic for the 
# conclusion section of the high level report
# Betweenness, closeness (harmonic centrality), Degree centrality, Eigen score

fun_cent_rank <- function(object, column) {
  
  {{object}} |> 
    select(value = {{column}}) |> 
    slice_max(order_by = value
              , n = 10) |> 
    rownames_to_column(var = "Organization") #|> 
  #select("Organization")
  }

#Joinbodi neighborhood stats
btwn_cent_joinbodi <- fun_cent_rank(joinbodi_nbhd_betweenness
                                    ,"igraph::betweenness(graph)")
deg_cent_joinbodi <- fun_cent_rank(joinbodi_nbhd_degree
                                   ,"igraph::degree(graph)")
harm_cent_joinbodi <- fun_cent_rank(joinbodi_nbhd_harmonic
                           ,"igraph::harmonic_centrality(graph)")
close_cent_joinbodi <- fun_cent_rank(joinbodi_nbhd_closeness
                                     , "igraph::closeness(graph, mode = \"all\")")
eigen_cent_joinbodi <- fun_cent_rank(joinbodi_nbhd_eigen, "igraph::eigen_centrality(graph)$vector")


#Criminal Justice neighborhood stats
btwn_cent_crimjust  <- fun_cent_rank(crimjust_nbhd_betweenness
                                    ,"igraph::betweenness(graph)")
deg_cent_crimjust <- fun_cent_rank(crimjust_nbhd_degree
                                   ,"igraph::degree(graph)")
harm_cent_crimjust <- fun_cent_rank(crimjust_nbhd_harmonic
                                    ,"igraph::harmonic_centrality(graph)")
close_cent_crimjust <- fun_cent_rank(crimjust_nbhd_closeness
                                     , "igraph::closeness(graph, mode = \"all\")")
eigen_cent_crimjust <- fun_cent_rank(crimjust_nbhd_eigen, "igraph::eigen_centrality(graph)$vector")


#Media & Journalism neighborhood stats
btwn_cent_medjourn  <- fun_cent_rank(medjourn_nbhd_betweenness
                                     ,"igraph::betweenness(graph)")
deg_cent_medjourn <- fun_cent_rank(medjourn_nbhd_degree
                                   ,"igraph::degree(graph)")
harm_cent_medjourn <- fun_cent_rank(medjourn_nbhd_harmonic
                                    ,"igraph::harmonic_centrality(graph)")
close_cent_medjourn <- fun_cent_rank(medjourn_nbhd_closeness
                                     , "igraph::closeness(graph, mode = \"all\")")
eigen_cent_medjourn <- fun_cent_rank(medjourn_nbhd_eigen, "igraph::eigen_centrality(graph)$vector")


#Behavior change neighborhood stats
btwn_cent_bechange  <- fun_cent_rank(bechange_nbhd_betweenness
                                     ,"igraph::betweenness(graph)")
deg_cent_bechange <- fun_cent_rank(bechange_nbhd_degree
                                   ,"igraph::degree(graph)")
harm_cent_bechange <- fun_cent_rank(bechange_nbhd_harmonic
                                    ,"igraph::harmonic_centrality(graph)")
close_cent_bechange <- fun_cent_rank(bechange_nbhd_closeness
                                     , "igraph::closeness(graph, mode = \"all\")")
eigen_cent_bechange <- fun_cent_rank(bechange_nbhd_eigen, "igraph::eigen_centrality(graph)$vector")


#identify the max rows for each neighborhood stat
#this exists because there are ties among the top 10s

max_rows_fun <- function(a, b, c, d) {
  
  max(nrow(a), nrow(b)
                  , nrow(c), nrow(d))
}

max_rows_joinbodi <- max_rows_fun(a = btwn_cent_joinbodi
                                  , b = deg_cent_joinbodi
                                  , c= close_cent_joinbodi
                                  , d = eigen_cent_joinbodi)

max_rows_crimjust <- max_rows_fun(a = btwn_cent_crimjust
                                  , b = deg_cent_crimjust
                                  , c= close_cent_crimjust
                                  , d = eigen_cent_crimjust)

max_rows_bechange <- max_rows_fun(a = btwn_cent_bechange
                                  , b = deg_cent_bechange
                                  , c= close_cent_bechange
                                  , d = eigen_cent_bechange)

max_rows_medjourn <- max_rows_fun(a = btwn_cent_medjourn
                                  , b = deg_cent_medjourn
                                  , c= close_cent_medjourn
                                  , d = eigen_cent_medjourn)


pad_rows <- function(df, n) {
  if (nrow(df) < n) {
    rbind(df, as.data.frame(matrix(NA, nrow = n - nrow(df), ncol = ncol(df), 
                                   dimnames = list(NULL, names(df)))))
  } else {
    df
  }
}

#Joinbodi top 10 lists with padding
btwn_cent_top10_joinbodi <- pad_rows(btwn_cent_joinbodi, max_rows_joinbodi)
harm_cent_top10_joinbodi <- pad_rows(harm_cent_joinbodi, max_rows_joinbodi)
deg_cent_top10_joinbodi <- pad_rows(deg_cent_joinbodi, max_rows_joinbodi)
close_cent_top10_joinbodi <- pad_rows(close_cent_joinbodi, max_rows_joinbodi)
eigen_cent_top10_joinbodi <- pad_rows(eigen_cent_joinbodi, max_rows_joinbodi)

#Criminal Justice top 10 lists with padding
btwn_cent_top10_crimjust <- pad_rows(btwn_cent_crimjust, max_rows_crimjust)
harm_cent_top10_crimjust <- pad_rows(harm_cent_crimjust, max_rows_crimjust)
deg_cent_top10_crimjust <- pad_rows(deg_cent_crimjust, max_rows_crimjust)
close_cent_top10_crimjust <- pad_rows(close_cent_crimjust, max_rows_crimjust)
eigen_cent_top10_crimjust <- pad_rows(eigen_cent_crimjust, max_rows_crimjust)

#Media and Journalism top 10 lists with padding
btwn_cent_top10_medjourn <- pad_rows(btwn_cent_medjourn, max_rows_medjourn)
harm_cent_top10_medjourn <- pad_rows(harm_cent_medjourn, max_rows_medjourn)
deg_cent_top10_medjourn <- pad_rows(deg_cent_medjourn, max_rows_medjourn)
close_cent_top10_medjourn <- pad_rows(close_cent_medjourn, max_rows_medjourn)
eigen_cent_top10_medjourn <- pad_rows(eigen_cent_medjourn, max_rows_medjourn)

#Behavior Change top 10 lists with padding
btwn_cent_top10_bechange <- pad_rows(btwn_cent_bechange, max_rows_bechange)
harm_cent_top10_bechange <- pad_rows(harm_cent_bechange, max_rows_bechange)
deg_cent_top10_bechange <- pad_rows(deg_cent_bechange, max_rows_bechange)
close_cent_top10_bechange <- pad_rows(close_cent_bechange, max_rows_bechange)
eigen_cent_top10_bechange <- pad_rows(eigen_cent_bechange, max_rows_bechange)


#This function assembles the data for each neighborhood
scores_table_fun <- function(a, b, c, d) {
  bind_cols(a, b, c, d) |> 
  select(Betweenness = "Organization...1"
         , Closeness = "Organization...3"
         , Degree = "Organization...5"
         , "Eigen Value" = "Organization...7")

}

#Assemble the data for each neighborhood
scores_table_joinbodi <- scores_table_fun(btwn_cent_top10_joinbodi
                                          , close_cent_top10_joinbodi
                                          , deg_cent_top10_joinbodi
                                          , eigen_cent_top10_joinbodi)

scores_table_crimjust <- scores_table_fun(btwn_cent_top10_crimjust
                                          , close_cent_top10_crimjust
                                          , deg_cent_top10_crimjust
                                          , eigen_cent_top10_crimjust)


scores_table_medjourn <- scores_table_fun(btwn_cent_top10_medjourn
                                          , close_cent_top10_medjourn
                                          , deg_cent_top10_medjourn
                                          , eigen_cent_top10_medjourn)

scores_table_bechange <- scores_table_fun(btwn_cent_top10_bechange
                                          , close_cent_top10_bechange
                                          , deg_cent_top10_bechange
                                          , eigen_cent_top10_bechange)


#Make the final tables  
table_10_joinbodi <- flextable(scores_table_joinbodi) |> 
  set_caption(caption = "Joinbodi: Top 10 Organizations by Centrality Statistic")

flex_fun(scores_table_joinbodi)

table_10_crimjust <- flextable(scores_table_crimjust) |> 
  set_caption(caption = "Criminal Justice: Top 10 Organizations \nby Centrality Statistic")

table_10_medjourn <- flextable(scores_table_medjourn) |> 
  set_caption(caption = "Media & Journalism: Top 10 Organizations \nby Centrality Statistic")

table_10_bechange <- flextable(scores_table_bechange) |> 
  set_caption(caption = "Behavior Change: Top 10 Organizations \nby Centrality Statistic")

#tables to compare actuals with simulated graphs
avg_net_stats_fun(graph = joinbodi_nbhd
                  , network = "Joinbodi Neighborhood"
                  , harm = joinbodi_nbhd_harmonic
                  , between = joinbodi_nbhd_betweenness
                  , eigen = joinbodi_nbhd_eigen)

avg_net_stats_fun(graph = bechange_nbhd
                  , network = "Behavior Change Neighborhood"
                  , harm = bechange_nbhd_harmonic
                  , between = bechange_nbhd_betweenness
                  , eigen = bechange_nbhd_eigen)

avg_net_stats_fun(graph = crimjust_nbhd
                  , network = "Criminal Justice Neighborhood"
                  , harm = crimjust_nbhd_harmonic
                  , between = crimjust_nbhd_betweenness
                  , eigen = crimjust_nbhd_eigen)

avg_net_stats_fun(graph = medjourn_nbhd
                  , network = "Media and Journalism Neighborhood"
                  , harm = medjourn_nbhd_harmonic
                  , between = medjourn_nbhd_betweenness
                  , eigen = medjourn_nbhd_eigen)

# Add nodes for visualization use

#JoinBodi
V(joinbodi_nbhd)$names_pretty <- str_wrap(V(joinbodi_nbhd)$name, width = 20)
V(joinbodi_nbhd)$group <- V(joinbodi_nbhd)$cohort
V(joinbodi_nbhd)$title <- paste0("<b>Org:</b> ", V(joinbodi_nbhd)$name, "<br><b>Cohort:</b> ", V(joinbodi_nbhd)$cohort
                                 , "<br><b>Degree:</b> ", V(joinbodi_nbhd)$degree, "<br><b>Between:</b> ", V(joinbodi_nbhd)$betweenness                     , "<br><b>Centrality:</b> ", V(joinbodi_nbhd)$centrality, "<br><b>Eigen:</b> ", V(joinbodi_nbhd)$eigen)
V(joinbodi_nbhd)$degree <- igraph::degree(joinbodi_nbhd)
V(joinbodi_nbhd)$betweenness <- round(igraph::betweenness(joinbodi_nbhd), digits = 2)
V(joinbodi_nbhd)$centrality <- round(igraph::harmonic_centrality(joinbodi_nbhd), digits = 2)
V(joinbodi_nbhd)$eigen <- round(igraph::eigen_centrality(joinbodi_nbhd)$vector, digits = 2)
V(joinbodi_nbhd)$value <- log10(igraph::degree(joinbodi_nbhd))
V(joinbodi_nbhd)$close <- round(igraph::closeness(joinbodi_nbhd), digits = 2)

# Behavior Change
V(bechange_nbhd)$names_pretty <- str_wrap(V(bechange_nbhd)$name, width = 20)
V(bechange_nbhd)$group <- V(bechange_nbhd)$cohort
V(bechange_nbhd)$title <- paste0("<b>Org:</b> ", V(bechange_nbhd)$name, "<br><b>Cohort:</b> ", V(bechange_nbhd)$cohort
                                 , "<br><b>Degree:</b> ", V(bechange_nbhd)$degree, "<br><b>Between:</b> ", V(bechange_nbhd)$betweenness
                                 , "<br><b>Centrality:</b> ", V(bechange_nbhd)$centrality, "<br><b>Eigen:</b> ", V(bechange_nbhd)$eigen)
V(bechange_nbhd)$degree <- igraph::degree(bechange_nbhd)
V(bechange_nbhd)$betweenness <- round(igraph::betweenness(bechange_nbhd), digits = 2)
V(bechange_nbhd)$centrality <- round(igraph::harmonic_centrality(bechange_nbhd), digits = 2)
V(bechange_nbhd)$eigen <- round(igraph::eigen_centrality(bechange_nbhd)$vector, digits = 2)
V(bechange_nbhd)$value <- log10(igraph::degree(bechange_nbhd))
V(bechange_nbhd)$close <- round(igraph::closeness(bechange_nbhd), digits = 2)

# Criminal Justice
V(crimjust_nbhd)$names_pretty <- str_wrap(V(crimjust_nbhd)$name, width = 20)
V(crimjust_nbhd)$group <- V(crimjust_nbhd)$cohort
V(crimjust_nbhd)$title <- paste0("<b>Org:</b> ", V(crimjust_nbhd)$name, "<br><b>Cohort:</b> ", V(crimjust_nbhd)$cohort
                                 , "<br><b>Degree:</b> ", V(crimjust_nbhd)$degree, "<br><b>Between:</b> ", V(crimjust_nbhd)$betweenness
                                 , "<br><b>Centrality:</b> ", V(crimjust_nbhd)$centrality, "<br><b>Eigen:</b> ", V(crimjust_nbhd)$eigen)
V(crimjust_nbhd)$degree <- igraph::degree(crimjust_nbhd)
V(crimjust_nbhd)$betweenness <- round(igraph::betweenness(crimjust_nbhd), digits = 2)
V(crimjust_nbhd)$centrality <- round(igraph::harmonic_centrality(crimjust_nbhd), digits = 2)
V(crimjust_nbhd)$eigen <- round(igraph::eigen_centrality(crimjust_nbhd)$vector, digits = 2)
V(crimjust_nbhd)$value <- log10(igraph::degree(crimjust_nbhd))
V(crimjust_nbhd)$close <- round(igraph::closeness(crimjust_nbhd), digits = 2)


# Media and Journalism
V(medjourn_nbhd)$names_pretty <- str_wrap(V(medjourn_nbhd)$name, width = 20)
V(medjourn_nbhd)$group <- V(medjourn_nbhd)$cohort
V(medjourn_nbhd)$title <- paste0("<b>Org:</b> ", V(medjourn_nbhd)$name, "<br><b>Cohort:</b> ", V(medjourn_nbhd)$cohort
                                 , "<br><b>Degree:</b> ", V(medjourn_nbhd)$degree, "<br><b>Between:</b> ", V(medjourn_nbhd)$betweenness
                                 , "<br><b>Centrality:</b> ", V(medjourn_nbhd)$centrality, "<br><b>Eigen:</b> ", V(medjourn_nbhd)$eigen)
V(medjourn_nbhd)$degree <- igraph::degree(medjourn_nbhd)
V(medjourn_nbhd)$betweenness <- round(igraph::betweenness(medjourn_nbhd), digits = 2)
V(medjourn_nbhd)$centrality <- round(igraph::harmonic_centrality(medjourn_nbhd), digits = 2)
V(medjourn_nbhd)$eigen <- round(igraph::eigen_centrality(medjourn_nbhd)$vector, digits = 2)
V(medjourn_nbhd)$value <- log10(igraph::degree(medjourn_nbhd))
V(medjourn_nbhd)$close <- round(igraph::closeness(medjourn_nbhd), digits = 2)


#testing centrality plot by neighborhood

#centrality plot by betweenness
subtitle <- "<span style = 'font-size:18pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:18pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:18pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:18pt; color:#5B507A;'>**JoinBodi Cohort**</span>" 

graph_nbhd_centrality_fun <- function(graph, title) {
ggraph(graph, layout = "centrality", cent = igraph::betweenness(graph))+
  geom_edge_bundle_force(edge_color = "lightgrey", edge_linewidth = .4, alpha = .8
                         , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                         , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = igraph::betweenness(graph), shape = grantee), color = "white") +
  geom_node_point(aes(size = igraph::betweenness(graph), shape = grantee, color = cohort)) +
  geom_node_text(aes(filter = igraph::betweenness(graph) >= quantile(igraph::betweenness(graph), .9), label = acronym), size = 6, repel = TRUE) +
  scale_size(range = c(1, 12)) +
  scale_color_manual(values = palette)+
  labs(title = title
       , subtitle = subtitle
       , shape = "Grantee"
       , size = "Degree")+
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom"
        , legend.text = element_text(size = 14)
        , legend.title = element_text(size = 16)
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")

}

#remove unconnected nodes from behavior change nbhd
comp <- igraph::components(bechange_nbhd)

largest_component_id <- which.max(comp$csize)

bechange_nbhd_main <- igraph::induced_subgraph(bechange_nbhd
                                        , vids = V(bechange_nbhd)[comp$membership == largest_component_id])

#remove unconnected nodes from criminal justice nbhd
comp1 <- igraph::components(crimjust_nbhd)

largest_component_id1 <- which.max(comp1$csize)

crimjust_nbhd_main <- igraph::induced_subgraph(crimjust_nbhd
                                               , vids = V(crimjust_nbhd)[comp1$membership == largest_component_id])
