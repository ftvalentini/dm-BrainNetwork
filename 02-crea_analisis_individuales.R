# crea/guarda objeto 'indiv' con informacion sobre las redes por sujeto por estadio por delta

# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# data --------------------------------------------------------------------

estadios = factor(c("W","N1","N2","N3")) %>% relevel(ref="W")
# redes individuales
redes_i = readRDS("data/working/redes_individuales.RDS")
# red promedio de cada estadio (m=mean)
redes_m = readRDS("data/working/redes_promedio.RDS")
# datos sobre las regiones
regiones = readRDS("data/working/regiones_data.RDS")
# combinaciones de deltas
deltas = seq(0.025, 0.15, 0.005)
# nombres de sujetos
sujetos = names(redes_i$N1)

# redes individuales --------------------------------------------

# medidas resumen por delta 
library(igraph)
library(brainGraph)
set.seed(1)
indiv = expand.grid(state=estadios, delta=deltas, sujeto=sujetos) %>%
  as_tibble() %>% 
  mutate(
    # matrices pesadas
    matriz = map2(state, sujeto, ~redes_i[[.x]][[.y]]),
    # matrices no pesadas         
    red_np = map2(matriz, delta, ~rednp(.x,.y)),
    # grafos no pesados
    grafo = map(red_np, ~graph.adjacency(.x,mode="undirected",diag=F)),
    # clusters (modulos) louvain
    modulos = map(grafo, cluster_louvain),
    # participation, z-score y rol de cada nodo
    part_coefs = map2(grafo, modulos, ~part_coeff(.x, .y$membership)),
    z_scores = map2(grafo, modulos, ~within_module_deg_z_score(.x, .y$membership)),
    roles = map2(part_coefs, z_scores, 
                 ~case_when(.x>0.05 & .y>1 ~ "hub",
                            .x<0.05 & .y>1 ~ "provincial_hub",
                            .x<0.05 & .y<1 ~ "provincial_node",
                            .x>0.05 & .y<1 ~ "connector_node")),
    # medidas resumen 
    modularidad = map2_dbl(grafo, modulos, ~modularity(.x, .y$membership)),
    nclusters = map_dbl(modulos, ~max(.x$membership)),
    hubs = map_dbl(roles, ~sum(.x %in% "hub")),
    provincialhubs  = map_dbl(roles, ~sum(.x %in% "provincial_hub")),
    provincialnodes = map_dbl(roles, ~sum(.x %in% "provincial_node")),
    connectornodes = map_dbl(roles, ~sum(.x %in% "connector_node"))
  )

# save rds ----------------------------------------------------------------

saveRDS(indiv, file="data/working/analisis_individuales.RDS")
