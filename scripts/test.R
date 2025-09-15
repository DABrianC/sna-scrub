


::: {.panel-tabset}

## Endline Network
```{r}

#| out-width: 100%
#| fig-cap: "The Endline Network"



ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(fill = module, size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = module)) +
  geom_node_text(aes(filter = degree >= 9, label = names_pretty), size = 4, repel = TRUE) +
  scale_size(range = c(1, 6)) +
  theme_graph() +
  theme(legend.position = "bottom")

```
#subgraphs
g_behavior <- induced_subgraph(g, V(g)[!is.na(group) & group == "Behavior Change"])
g_dont <- induced_subgraph(g, V(g)[!is.na(group) & group == "Media and Journalism"])

g_crim <- induced_subgraph(g, V(g)[!is.na(group) & group == "Criminal Justice"])

g_join <- induced_subgraph(g, V(g)[!is.na(group) & group == "JoinBodi"])

ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(fill = module, size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = module)) +
  #geom_node_text(aes(filter = degree >= 9, label = names_pretty), size = 4, repel = TRUE) +
  scale_size(range = c(1, 6)) +
  theme_graph() +
  theme(legend.position = "bottom")

subtitle <- "<span style = 'color:#F8766D;'>Behavior Change Module</span>
,<span style = 'color:#F564E3;'>Media and Journalism Module</span>
,<span style = 'color:#B79F00;'>Criminal Justice Module</span>
,<span style = 'color:#619CFF;'>JoinBodi Module</span>"

#Main graph
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(fill = module, size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = module)) +
  #geom_node_text(aes(filter = degree >= 9, label = names_pretty), size = 4, repel = TRUE) +
  scale_size(range = c(1, 6)) +
  labs(title = "The Whole Network"
       , subtitle = subtitle)+
  theme_graph() +
  theme(legend.position = "bottom"
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")
  
#behavior change subgraph on g
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white")+
  geom_node_point(aes(size = degree, shape = grantee, color = ifelse(group == "Behavior Change", "Behavior Change", "Other")))+
  geom_node_text(aes(label = ifelse(group == "Behavior Change", names_pretty, NA)), size = 3, repel = TRUE) +
  scale_color_manual(values = c("Behavior Change" = palette[[1]], "Other" = "lightgrey")) +
  scale_size(range = c(1, 6)) +
  labs(title = "<span style = 'color:#F8766D;'>Behavior Change Module</span>")+
  theme_graph() +
  theme(legend.position = "none"
        , plot.title.position = "plot"
        , plot.title = element_markdown())

#Media and Journalism subgraph on g
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white")+
  geom_node_point(aes(size = degree, shape = grantee, color = ifelse(group == "Media and Journalism", "Media and Journalism", "Other")))+
  geom_node_text(aes(label = ifelse(group == "Media and Journalism", names_pretty, NA)), size = 3, repel = TRUE) +
  scale_color_manual(values = c("Media and Journalism" = palette[[6]], "Other" = "lightgrey")) +
  scale_size(range = c(1, 6)) +
  labs(title = "<span style = 'color:#F564E3;'>Media and Journalism Module</span>")+
  theme_graph() +
  theme(legend.position = "none"
        , plot.title.position = "plot"
        , plot.title = element_markdown())

#Criminal Justice subgraph on g
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white")+
  geom_node_point(aes(size = degree, shape = grantee, color = ifelse(group == "Criminal Justice", "Criminal Justice", "Other")))+
  geom_node_text(aes(label = ifelse(group == "Criminal Justice", names_pretty, NA)), size = 3, repel = TRUE) +
  scale_color_manual(values = c("Criminal Justice" = palette[[2]], "Other" = "lightgrey")) +
  scale_size(range = c(1, 6)) +
  labs(title = "<span style = 'color:#B79F00;'>Criminal Justice Module</span>")+
  theme_graph() +
  theme(legend.position = "none"
        , plot.title.position = "plot"
        , plot.title = element_markdown())

#JoinBodi subgraph on g
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white")+
  geom_node_point(aes(size = degree, shape = grantee, color = ifelse(group == "JoinBodi", "JoinBodi", "Other")))+
  geom_node_text(aes(label = ifelse(group == "JoinBodi", names_pretty, NA)), size = 3, repel = TRUE) +
  scale_color_manual(values = c("JoinBodi" = palette[[5]], "Other" = "lightgrey")) +
  scale_size(range = c(1, 6)) +
  labs(title = "<span style = 'color:#619CFF;'>JoinBodi Module</span>")+
  theme_graph() +
  theme(legend.position = "none"
        , plot.title.position = "plot"
        , plot.title = element_markdown())


unique(V(g)$name)


#backbone layout

library(graphlayouts)
library(oaqc)

set.seed(665)
# create network with a group structure
network <- sample_islands(9, 40, 0.4, 15)
network <- simplify(network)

network <- simplify(g)
bb <- layout_as_backbone(network, keep = 0.2)
E(network)$col <- FALSE
E(network)$col[bb$backbone] <- TRUE

ggraph(network, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(col = col), width = 0.1) +
  geom_node_point(aes(col = grp)) +
  scale_color_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph() +
  theme(legend.position = "none")

# testing a ggraph object
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(size = degree, shape = grantee), color = "white")+
  geom_node_point(aes(size = degree, shape = grantee, color = ifelse(group == "Criminal Justice", "Criminal Justice", "Other")))+
  geom_node_text(aes(label = ifelse(group == "Criminal Justice", names_pretty, NA)), size = 3, repel = TRUE) +
  scale_color_manual(values = c("Criminal Justice" = palette[[2]], "Other" = "lightgrey")) +
  scale_size(range = c(1, 6)) +
  labs(title = "<span style = 'color:#B79F00;'>Criminal Justice Cohort</span>")+
  theme_graph() +
  theme(legend.position = "none"
        , plot.title.position = "plot"
        , plot.title = element_markdown())

# testing main graph colors
ggraph(g, layout = "stress") +
  geom_edge_link0(edge_color = "grey66", edge_linewidth = .2, alpha = .6
                  , arrow = arrow(angle = 15, length = unit(.05, "inches")
                                  , ends = "last", type = "closed")) +
  geom_node_point(aes(fill = cohort, size = degree, shape = grantee), color = "white") +
  geom_node_point(aes(size = degree, shape = grantee, color = cohort), alpha = .6) +
  #geom_node_text(aes(filter = degree >= 9, label = names_pretty), size = 4, repel = TRUE) +
  scale_size(range = c(1, 6)) +
  scale_color_manual(values = palette)+
  labs(title = "The Whole Network"
       , subtitle = subtitle)+
  theme_graph() +
  theme(legend.position = "bottom"
        , plot.subtitle = element_markdown()) +
  guides(color = "none", fill = "none")
