# crea/guarda objeto 'prom' con informacion sobre las redes promedio por estadio por delta

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

# redes promedio - medidas resumen por delta ------------------------------

library(igraph)
library(brainGraph)
set.seed(1)
prom = expand.grid(state = estadios, delta = deltas) %>%
  as_tibble() %>% 
  mutate(
    # matrices promedio pesadas
    matriz = rep(redes_m, length(deltas)),
    # matrices promedio no pesadas         
    red_np = map2(matriz, delta, ~rednp(.x,.y)),
    # grafos no pesados
    grafo = map(red_np, ~graph.adjacency(.x,mode="undirected",diag=F)),
    # clusters (modulos) louvain
    modulos = map(grafo, cluster_louvain),
    # grafo random con igual distribucion de grado
    g_rand_eqd = map(grafo, ~rewire(.x, keeping_degseq(niter=100))),
    modulos_g_rand_eqd = map(g_rand_eqd, cluster_louvain),
    modularidad_g_rand_eqd = map2_dbl(g_rand_eqd, modulos_g_rand_eqd,
                                      ~modularity(.x, .y$membership)),
    nclusters_g_rand_eqd = map_dbl(modulos_g_rand_eqd, ~max(.x$membership)),
    # medidas resumen 
    grado = map_dbl(grafo, ~(degree(.x) %>% mean)),
    cercania = map_dbl(grafo, ~closeness(.x) %>% mean),
    autovec = map_dbl(grafo, ~eigen_centrality(.x)$vector %>% mean),
    intermed = map_dbl(grafo, ~betweenness(.x) %>% mean),
    diametro = map_dbl(grafo, ~diameter(.x)),
    clustering = map_dbl(grafo, ~transitivity(.x, type="global")),
    modularidad = map2_dbl(grafo, modulos, ~modularity(.x, .y$membership)),
    nclusters = map_dbl(modulos, ~max(.x$membership)),
    # roles (solo para hacer visualizacion)
    part_coefs = map2(grafo, modulos, ~part_coeff(.x, .y$membership)),
    z_scores = map2(grafo, modulos, ~within_module_deg_z_score(.x, .y$membership)),
    roles = map2(part_coefs, z_scores, 
                 ~case_when(.x>0.05 & .y>1 ~ "H",
                            .x<0.05 & .y>1 ~ "PH",
                            .x<0.05 & .y<1 ~ "PN",
                            .x>0.05 & .y<1 ~ "CN")),
  ) %>%
  # AGREGAR/SACAR segun SI SE HACE CON EL PROMEDIO o por POR SUJETO
  # similaridad de modulos entre N* y W por delta (RAND index)
  group_by(delta) %>%
  # modulos de estado W por delta
  mutate(mod_w = if_else(state=="W",modulos,NULL)) %>%
  fill(mod_w) %>%
  ungroup %>%
  mutate(
    # RAND index N* vs W
    rand_w = map2_dbl(modulos, mod_w, ~compare(.x, .y, method="adjusted.rand")),
    # 1000 permutaciones de modulos para cada grafo (matriz 116x1000) 
    random_mods1 = map(modulos, ~replicate(1000, sample(.x$membership))),
    # otras 1000 permutaciones de modulos para cada grafo (matriz 116x1000) 
    random_mods2 = map(modulos, ~replicate(1000, sample(.x$membership))),
    #### VER CUAL CORRESPONDE:
    # rand index entre modulos simulados y modulos efectivos (por columna)
    rand_azar = map2(random_mods1, modulos, 
                     ~apply(.x, 2, function(col) 
                       compare(col, .y$membership, method="adjusted.rand"))),
    # # rand index entre modulos simulados para ambos grupos (por columna)
    # rand_azar = map2(
    #   random_mods1, random_mods2,
    #   ~mat_apply_bin(mat1=.x, mat2=.y, marg=2,
    #                  fn=function(a,b) compare(a,b,method="adjusted.rand"))
    # ),
    # proporcion de rand_azar que superan a rand verdadero (p-valor)
    pval_rand = map2_dbl(rand_azar, rand_w, ~mean(.x>.y))
  )
### OJO closeness (grafo no conexo --- ver documentacion igraph)
### se puede agregar excentricity como medida de centralidad
### grafo random tambien se puede hacer con:
# g_rand_eqd = map(grafo, ~sample_degseq(degree(.x)))
### no tiene sentido ajustar p.valores porque dan todos cero...

# save rds ----------------------------------------------------------------

saveRDS(prom, file="data/working/analisis_promedio.RDS")
