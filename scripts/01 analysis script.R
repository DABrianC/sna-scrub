
source(here::here("./prep/prep.R"))
source(here::here("./scripts/00 cleaning script.R"))

# Adding wrapped organization names to the dataset

#Descriptives

# Grantee survey analysis ----

#Centrality Statistics Endline
degree_end <- igraph::degree(g) |> 
  as.data.frame()

degree_end_out <- igraph::degree(g, mode = "out") |> 
  as.data.frame()

degree_end_in <- igraph::degree(g, mode = "in") |> 
  as.data.frame()

strength_end <- igraph::strength(g) |> 
  as.data.frame()

density_end <- igraph::edge_density(g) |> 
  as.data.frame()

between_end <- igraph::betweenness(g) |> 
  as.data.frame()

eigen_central_end <- igraph::eigen_centrality(g)$vector |> 
  as.data.frame()

transitivity_end <- igraph::transitivity(g) |> 
  as.data.frame()

harmonic_end <- igraph::harmonic_centrality(g) |> 
  as.data.frame()

closeness_end <- igraph::closeness(g, mode = "all") |> 
  as.data.frame()

centrality_end <- bind_cols(between_end, harmonic_end 
                  , eigen_central_end, degree_end
                  , degree_end_out, degree_end_in) |>   
  rownames_to_column(var = "Organization") |> 
  rename("Betweenness" = "igraph::betweenness(g)"
         , "Harmonic Centrality" = "igraph::harmonic_centrality(g)"
         , "Eigen" = "igraph::eigen_centrality(g)$vector"
         , "Degree" = "igraph::degree(g)"
         , "Out-degree" = "igraph::degree(g, mode = \"out\")"
         , "In-degree" = "igraph::degree(g, mode = \"in\")")


centrality_end <- centrality_end |> 
  group_by(Organization) |> 
  summarize(
    across(.cols = 1:6
           , .fns = ~round(.x, digits = 2))
  )

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

btwn_cent <- fun_cent_rank(between_end, "igraph::betweenness(g)")

deg_cent <- fun_cent_rank(degree_end, "igraph::degree(g)")
  
harm_cent <- fun_cent_rank(harmonic_end, "igraph::harmonic_centrality(g)") 

close_cent <- fun_cent_rank(closeness_end, "igraph::closeness(g, mode = \"all\")")
#eigen_cent <- fun_cent_rank(eigen_central_end, "igraph::eigen_centrality(g)$vector")

max_rows <- max(nrow(btwn_cent), nrow(close_cent)
                     , nrow(deg_cent))#, nrow(eigen_cent)) 

pad_rows <- function(df, n) {
  if (nrow(df) < n) {
    rbind(df, as.data.frame(matrix(NA, nrow = n - nrow(df), ncol = ncol(df), 
                                   dimnames = list(NULL, names(df)))))
  } else {
    df
  }
}

btwn_cent1 <- pad_rows(btwn_cent, max_rows)
harm_cent1 <- pad_rows(harm_cent, max_rows)
deg_cent1 <- pad_rows(deg_cent, max_rows)
close_cent1 <- pad_rows(close_cent, max_rows)
#eigen_cent1 <- pad_rows(eigen_cent, max_rows)



scores_table <- bind_cols(btwn_cent1, close_cent1,
                          deg_cent1) |> #, eigen_cent1) |> 
  select(Betweenness = "Organization...1"
         , Closeness = "Organization...3"
         , Degree = "Organization...5")
         #, "Eigen Value" = "Organization...7")

table_10 <- flextable(scores_table) |> 
          set_caption(caption = "Top 10 Organizations by Centrality Statistic")

#Calculate rankings across each of the statistics
#centrality_end_rank <- centrality_end |> 
#  group_by(Organization) |> 
#  summarize(
 #   across(.cols = 1:4
 #          , .fns = ~rank(.x, ties.method = "max")
 #          )
 # )

 # ties are averaged
# Neighborhood inclusion


#P1 <- netrankr::neighborhood_inclusion(g)

#V(g)$names_pretty <- str_wrap(V(g)$name, width = 20)
#V(g)$group <- V(g)$cohort
#V(g)$title <- paste0("<b>Org:</b> ", V(g)$name, "<br><b>Cohort:</b> ", V(g)$cohort
##                     , "<br><b>Degree:</b> ", V(g)$degree, "<br><b>Between:</b> ", V(g)$betweenness                     , "<br><b>Centrality:</b> ", V(g)$centrality, "<br><b>Eigen:</b> ", V(g)$eigen)
#V(g)$degree <- igraph::degree(g)
#V(g)$betweenness <- round(igraph::betweenness(g), digits = 2)
#V(g)$centrality <- round(igraph::harmonic_centrality(g), digits = 2)
#V(g)$eigen <- round(igraph::eigen_centrality(g)$vector, digits = 2)
#V(g)$log_degree <- log10(igraph::degree(g))
#V(g)$close <- round(igraph::closeness(g), digits = 2)

#igraph <- toVisNetworkData((g))
#g_visnetwork <- visNetwork(nodes = igraph$nodes 
#           , edges = igraph$edges
#           , main = "Endline Network") |> 
 # visEdges(arrows = "to") |> 
#  visLegend()
  #visGroups(groupname = "JoinBodi", color = "darkblue")

#visIgraph(igraph = g
#          , layout = "layout_nicely"
#          )

# Grantee and non-grantee survey anlaysis----

#Centrality Statistics grantee and non-grantee endline
degree_non_end <- igraph::degree(g_non) |> 
  as.data.frame()

degree_non_end_out <- igraph::degree(g_non, mode = "out") |> 
  as.data.frame()

degree_non_end_in <- igraph::degree(g_non, mode = "in") |> 
  as.data.frame()

strength_non_end <- igraph::strength(g_non) |> 
  as.data.frame()

density_non_end <- igraph::edge_density(g_non) |> 
  as.data.frame()

between_non_end <- igraph::betweenness(g_non) |> 
  as.data.frame()

eigen_central_non_end <- igraph::eigen_centrality(g_non)$vector |> 
  as.data.frame()

transitivity_non_end <- igraph::transitivity(g_non) |> 
  as.data.frame()

harmonic_non_end <- igraph::harmonic_centrality(g_non) |> 
  as.data.frame()

closeness_non_end <- igraph::closeness(g_non, mode = "all") |> 
  as.data.frame()

centrality_non_end <- bind_cols(between_non_end, harmonic_non_end 
                                , eigen_central_non_end, degree_non_end
                                , degree_non_end_out, degree_non_end_in) |>   
  rownames_to_column(var = "Organization") |> 
  rename("Betweenness" = "igraph::betweenness(g_non)"
         , "Harmonic Centrality" = "igraph::harmonic_centrality(g_non)"
         , "Eigen" = "igraph::eigen_centrality(g_non)$vector"
         , "Degree" = "igraph::degree(g_non)"
         , "Out-degree" = "igraph::degree(g_non, mode = \"out\")"
         , "In-degree" = "igraph::degree(g_non, mode = \"in\")")


centrality_non_end <- centrality_non_end |> 
  group_by(Organization) |> 
  summarize(
    across(.cols = 1:6
           , .fns = ~round(.x, digits = 2))
  )

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

btwn_cent_non <- fun_cent_rank(between_non_end, "igraph::betweenness(g_non)")

deg_cent_non <- fun_cent_rank(degree_non_end, "igraph::degree(g_non)")

harm_cent_non <- fun_cent_rank(harmonic_non_end, "igraph::harmonic_centrality(g_non)") 

close_cent_non <- fun_cent_rank(closeness_non_end, "igraph::closeness(g_non, mode = \"all\")")
#eigen_cent <- fun_cent_rank(eigen_central_end, "igraph::eigen_centrality(g)$vector")

max_rows_non <- max(nrow(btwn_cent_non), nrow(close_cent_non)
                    , nrow(deg_cent_non))#, nrow(eigen_cent)) 

pad_rows <- function(df, n) {
  if (nrow(df) < n) {
    rbind(df, as.data.frame(matrix(NA, nrow = n - nrow(df), ncol = ncol(df), 
                                   dimnames = list(NULL, names(df)))))
  } else {
    df
  }
}

btwn_cent1_non <- pad_rows(btwn_cent_non, max_rows_non)
harm_cent1_non <- pad_rows(harm_cent_non, max_rows_non)
deg_cent1_non <- pad_rows(deg_cent_non, max_rows_non)
close_cent1_non <- pad_rows(close_cent_non, max_rows_non)
#eigen_cent1 <- pad_rows(eigen_cent, max_rows)



scores_table_non <- bind_cols(btwn_cent1_non, close_cent1_non,
                              deg_cent1_non) |> #, eigen_cent1) |> 
  select(Betweenness = "Organization...1"
         , Closeness = "Organization...3"
         , Degree = "Organization...5")
#, "Eigen Value" = "Organization...7")

table_10_non <- flextable(scores_table_non) |> 
  set_caption(caption = "Top 10 Organizations by Centrality Statistic")

#Calculate rankings across each of the statistics
#centrality_end_rank <- centrality_end |> 
#  group_by(Organization) |> 
#  summarize(
#   across(.cols = 1:4
#          , .fns = ~rank(.x, ties.method = "max")
#          )
# )

# ties are averaged
# Neighborhood inclusion


#P1 <- netrankr::neighborhood_inclusion(g)

V(g_non)$names_pretty <- str_wrap(V(g_non)$name, width = 20)
V(g_non)$group <- V(g_non)$cohort
V(g_non)$title <- paste0("<b>Org:</b> ", V(g_non)$name, "<br><b>Cohort:</b> ", V(g_non)$cohort
                         , "<br><b>Degree:</b> ", V(g_non)$degree, "<br><b>Between:</b> ", V(g_non)$betweenness                     , "<br><b>Centrality:</b> ", V(g)$centrality, "<br><b>Eigen:</b> ", V(g)$eigen)
V(g_non)$degree <- igraph::degree(g_non)
V(g_non)$betweenness <- round(igraph::betweenness(g_non), digits = 2)
V(g_non)$closeness <- round(igraph::closeness(g_non), digits = 2)
V(g_non)$eigen <- round(igraph::eigen_centrality(g_non)$vector, digits = 2)
V(g_non)$log_degree <- log10(igraph::degree(g_non))
V(g_non)$close <- round(igraph::closeness(g_non), digits = 2)

V(g_non)$color <- ifelse(V(g_non)$cohort == "Behavior Change", "#0964B0" 
                         , ifelse(V(g_non)$cohort == "Criminal Justice", "#E17F48"
                                  , ifelse(V(g_non)$cohort == "JoinBodi", "#5B507A"
                                           , ifelse(V(g_non)$cohort == "Media and Journalism", "#B5984F"
                                                    , "grey"))))

V(g_non)$shape <- ifelse(V(g_non)$grantee == "Non Grantee", "triangle", "dot")

color_map <- c(`Behavior \nChange` = "#0964B0"
               , `Criminal \nJustice` = "#E17F48"
               , `JoinBodi` = "#5B507A"
               , `Media and \nJournalism` = "#B5984F"
               , `Non \nGrantee` = "grey")

igraph <- toVisNetworkData((g_non))

igraph$nodes$color <- igraph$nodes$color.background <- igraph$nodes$color

igraph$nodes$shape <- V(g_non)$shape

legend_data <- unique(igraph$nodes[, c("shape", "color")])

#visNetwork(nodes = igraph$nodes 
 #          , edges = igraph$edges
 #          , main = "Endline Network") |>
 # visEdges(arrows = "to") |> 
 #            , selectedBy = "group"
 #            , nodesIdSelection = TRUE) |> 
  #visLegend(useGroups = FALSE
  #          , addNodes= data.frame(
  #            label = str_wrap(names(color_map), width = 20)
  #            , shape = legend_data$shape
  #            , color = unname(color_map)
  #         )) |> 
  #visLayout(randomSeed = 415)


#igraph <- toVisNetworkData((g_non))
#g_visnetwork <- visNetwork(nodes = igraph$nodes 
 #                          , edges = igraph$edges
 #                          , main = "Endline Network") |> 
 # visEdges(arrows = "to") |> 
 # visLegend() 

# Simulated networks

#function to calculate average network 
# statistics

avg_net_stats_fun <- function(graph, network, harm, between, eigen) {
  graph_name <- deparse(substitute(graph))
  
  
  g_non_sim <- vector('list', 1000)
  
  for(i in 1:1000){
    g_non_sim[[i]] <- erdos.renyi.game(
      n = gorder(graph)
      , p.or.m = edge_density(graph)
      , type = "gnp"
    )
  }

  g_non_sim_density <- mean(unlist(lapply(g_non_sim, edge_density)))
  g_non_sim_avg_between <- mean(unlist(lapply(g_non_sim, igraph::betweenness)))
  g_non_sim_avg_close <- mean(unlist(lapply(g_non_sim, igraph::closeness)))
  g_non_sim_avg_harmonic <- mean(unlist(lapply(g_non_sim, igraph::harmonic_centrality)))
  g_non_sim_avg_degree <- mean(unlist(lapply(g_non_sim, igraph::degree)))
  g_non_sim_eigen <- do.call(rbind, lapply(g_non_sim, eigen_centrality, directed = FALSE)) |> 
    data.frame()
  
  g_non_sim_avg_eigen <- mean(unlist(g_non_sim_eigen$vector))
  
  #averages of actual data
  density_stat <- edge_density(graph)
  harmonic_non_end_mean <- mean(harm[,1])
  between_non_end_mean <- mean(between[,1])
  eigen_central_non_end_mean <- mean(eigen[,1])
  
  
  #make a flextable
  names <- c("Network", "Density", "Avg. Harmonic Centrality \n(Closeness)", "Avg. Betweenness", "Avg. Eigenvector")
  g_non_stats <- c(paste0(network, " Network"), density_stat, harmonic_non_end_mean, between_non_end_mean, eigen_central_non_end_mean)
  
  sim_stats <- c("Simulated Networks", g_non_sim_density, g_non_sim_avg_harmonic, g_non_sim_avg_between, g_non_sim_avg_eigen)
  
  dat_all <- as.data.frame(rbind(setNames(g_non_stats, names)
                                 , setNames(sim_stats, names))) |> 
    mutate(across(2:5, as.numeric)
           , Network = as.character(Network))
  
  #graph statistics table
  network_stats_table <- flextable(dat_all) |>
    set_header_labels(values = names(dat_all)) |> 
    align(align = "center", part = "all") |>
    colformat_double(digits = 2)

assign(paste0(graph_name, "_density"), density_stat, envir = .GlobalEnv)  
assign(paste0(graph_name, "_harmonic__mean"), harmonic_non_end_mean, envir = .GlobalEnv) 
assign(paste0(graph_name, "_network_stats_table"), network_stats_table, envir = .GlobalEnv)
assign(paste0(graph_name, "_harmonic__mean"), harmonic_non_end_mean, envir = .GlobalEnv)  
assign(paste0(graph_name, "_between_mean"), between_non_end_mean, envir = .GlobalEnv)
assign(paste0(graph_name, "_eigen_central_mean"), eigen_central_non_end_mean, envir = .GlobalEnv)
assign(paste0(graph_name, "_g_sim"), g_non_sim , envir = .GlobalEnv)
assign(paste0(graph_name, "_dat_all"), dat_all, envir = .GlobalEnv)
assign(paste0(graph_name, "_stats"), g_non_stats, envir = .GlobalEnv)
assign(paste0(graph_name, "_sim_stats"), sim_stats, envir = .GlobalEnv)
assign(paste0(graph_name, "_sim"), g_non_sim, envir = .GlobalEnv)
}


avg_net_stats_fun(graph = g_non
                  , network = "Endline"
                  , harm = harmonic_non_end
                  , between = between_non_end
                  , eigen = eigen_central_non_end)


#simulation of endline network 1000x
g_non_sim <- vector('list', 1000)

for(i in 1:1000){
  g_non_sim[[i]] <- erdos.renyi.game(
    n = gorder(g_non)
    , p.or.m = edge_density(g_non)
    , type = "gnp"
  )
  
  
}

#averages of simulated data
g_non_sim_density <- mean(unlist(lapply(g_non_sim, edge_density)))
g_non_sim_avg_between <- mean(unlist(lapply(g_non_sim, igraph::betweenness)))
g_non_sim_avg_close <- mean(unlist(lapply(g_non_sim, igraph::closeness)))
g_non_sim_avg_harmonic <- mean(unlist(lapply(g_non_sim, igraph::harmonic_centrality)))
g_non_sim_avg_degree <- mean(unlist(lapply(g_non_sim, igraph::degree)))
g_non_sim_eigen <- do.call(rbind, lapply(g_non_sim, eigen_centrality, directed = FALSE)) |> 
  data.frame()

g_non_sim_avg_eigen <- mean(unlist(g_non_sim_eigen$vector))



#g_non_sim_dist <- mean(unlist(lapply(g_non_sim, mean_distance, directed = FALSE)))

#gl_ment_avg_trans <- mean(unlist(lapply(gl, transitivity)))

#averages of g_non network stats

harmonic_non_end_mean <- mean(harmonic_non_end[,1])
between_non_end_mean <- mean(between_non_end[,1])
eigen_central_non_end_mean <- mean(eigen_central_non_end[,1])

#make a flextable
names <- c("Network", "Density", "Avg. Harmonic Centrality \n(Closeness)", "Avg. Betweenness", "Avg. Eigenvector")
g_non_stats <- c("Endline Network", density_non_end, harmonic_non_end_mean, between_non_end_mean, eigen_central_non_end_mean)

sim_stats <- c("Simulated Networks", g_non_sim_density, g_non_sim_avg_harmonic, g_non_sim_avg_between, g_non_sim_avg_eigen)

dat_all <- as.data.frame(rbind(setNames(g_non_stats, names)
                        , setNames(sim_stats, names))) |> 
  mutate(across(2:5, as.numeric)
         , Network = as.character(Network))


#graph statistics table
network_stats_table <- flextable(dat_all) |>
  set_header_labels(values = names(dat_all)) |> 
  align(align = "center", part = "all") |>
  colformat_double(digits = 2)

network_stats_table

## cross community nodes----

V(g_non)$community <- V(g_non)$cohort 

#To help with fitting names in a column
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
V(g_non)$label <- ifelse(V(g_non)$name %in% cross_community_nodes, V(g_non)$names_pretty, NA)

subtitle <- "<span style = 'font-size:18pt; color:#0964B0;'>**Behavior Change Cohort**</span>, <span style = 'font-size:18pt; color:#B5984F;'>**Media and Journalism Cohort**</span>, <span style = 'font-size:18pt; color:#E17F48;'>**Criminal Justice Cohort**</span>, <br><span style = 'font-size:18pt; color:#5B507A;'>**JoinBodi Cohort**</span>"

ggraph(g_non, "stress") +
  geom_edge_link0(aes(alpha = !involves_excluded), edge_color = "white"
                  , edge_linewidth = .4
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = cohort)) +
  
  #foreground
  geom_edge_link0(aes(alpha = cross_community, color = cross_community), width = 1)+
  #, edge_linewidth = ifelse(color = TRUE, 2, .5))
  #, edge_linewidth = .4
  #, alpha = .8
  #  , arrow = arrow(angle = 15, length = unit(.05, "inches")
  #                  , ends = "last", type = "closed"), threshold = .4, n_cycle = 2) +
  geom_node_point(aes(size = degree, shape = grantee, color = cohort)) +
  #geom_node_point(data = ggraph(g_non_delete, "stress"), aes(size = degree, shape = grantee, color = cohort)) +
  geom_node_text(aes(label = label), size = 4, repel = TRUE
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
  guides(color = "none", fill = "none")


# plot of densities of nbhds and overall



 