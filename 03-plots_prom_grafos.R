# Visualizacion del grafo promedio no ponderado (para un delta)

# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# read data ----------------------------------------------------------------

# objeto con info de redes promedio por estadio por delta
prom = readRDS(file="data/working/analisis_promedio.RDS")
# datos de regiones
reg = readRDS(file="data/working/regiones_data.RDS")
# estadios
estadios = c("W","N1","N2","N3")

# funcion de visualizacion ---------------------------------------------------

library(igraph)
# layout alternativo a corte del cerebro (fijo para cualquier delta-estadio):
set.seed(1)
l_alt = layout_nicely((prom %>% dplyr::filter(near(delta,0.075) & state=="W"))$grafo[[1]])
# funcion
# plot de red promedio para densidad y estadio, con color y label ("area","hemis", "roles" o "modulos")

color_by="modulos"
densidad=0.075
estadio="N1"

grafo_plot = function(data_obj=prom, desc_obj=reg,
                      color_by=NULL, label_by=NULL, size_by_rol=F, layout_brain=T, legends=T,
                      densidad, estadio) {
  net = (data_obj %>% dplyr::filter(near(delta,densidad) & state==estadio))$grafo[[1]]
  roles = (data_obj %>% dplyr::filter(near(delta,densidad) & state==estadio))$roles[[1]]
  modulos = (data_obj %>% dplyr::filter(near(delta,densidad) & state==estadio))$modulos[[1]]$membership
  desc_obj[['roles']] = roles
  desc_obj[['modulos']] = modulos
  # nombres de regiones para poder matchear
  V(net)$name = desc_obj$region
  # vertex colors
  if (is.null(color_by)) {
    vercol = "navy"
    edgecol = "gray"
  } else {
    values_colorby = unique(desc_obj[[color_by]]) %>% sort()
    colrs <- RColorBrewer::brewer.pal(length(values_colorby), "Set1") %>%
      setNames(values_colorby)
    colrs[length(colrs)] = "black"
    vercol = colrs[match(desc_obj[[color_by]], names(colrs))]
    # edge color based on vertices involved
    puntas = ends(net, es=E(net), names=T)
    regstart <- tibble(region=puntas[,1]) %>%
      left_join(desc_obj, by="region") %>% select(color_by) %>% unlist()
    regeend <- tibble(region=puntas[,2]) %>%
      left_join(desc_obj, by="region") %>% select(color_by) %>% unlist()
    edgecol = case_when(regstart==regeend ~ colrs[regstart],
                        TRUE ~ "gray")
  }
  # vertex labels
  if (is.null(label_by)) verlab = "" else verlab = desc_obj[[label_by]]
  # vertex size by rol (if T)
  if (size_by_rol==T) {
    sizes = c(0.5,1.5,3,6) %>% setNames(c("H","PH","PN","CN"))
    versize = sizes[match(roles, names(sizes), nomatch=1)]
  } else {
    versize = 4
  }
  # layout del brain (segun atlas AAL116)
  if (layout_brain) l = cbind(brainGraph::aal116$x.mni, brainGraph::aal116$y.mni) else {
    l = l_alt
  }
  # plot
  plot(net, layout=l,
       edge.color=edgecol,
       vertex.size=versize,
       vertex.label=verlab,
       vertex.color=vercol,
       edge.curved=0.1)
  if (!is.null(color_by) & legends) {
    par(xpd=T)
    legend("right", legend=names(colrs), col=colrs,
           cex=2.5, bty="n", pch=20, pt.cex=4, inset=-0.21, y.intersp=1.2,
           horiz=F)
  }
  if (legends) title(estadio, adj=0, cex.main=2.5, line=-6)
}

# save plots (areas) --------------------------------------------------------------------

# 4 vizs por delta
deltas_plot = c(0.025,0.075,0.15)
# 4 plots-brain por delta
for(d in deltas_plot) {
  png("output/plots/redes_brain_"%+%str_remove(d,"\\.")%+%".png", width=1600, height=1200)
  par(mfrow=c(2,2))
  par(mai=c(0,0,0,2))
  walk(estadios, ~grafo_plot(densidad=d, estadio=.x, color_by="area"))
  dev.off()
}

# 4 plots-nobrain por delta
for(d in deltas_plot) {
  png("output/plots/redes_nobrain_"%+%str_remove(d,"\\.")%+%".png", width=1600, height=1200)
  par(mfrow=c(2,2))
  par(mai=c(0,0,0,2))
  walk(estadios, ~grafo_plot(densidad=d, estadio=.x, color_by="area", layout_brain=F))
  dev.off()
}

# save plots (roles) --------------------------------------------------------------------

delta_roles = 0.075
png("output/plots/redes_roles_nobrain_"%+%str_remove(delta_roles,"\\.")%+%".png",
    width=1600, height=1200)
par(mfrow=c(2,2))
par(mai=c(0,0,0,2))
walk(estadios, ~grafo_plot(densidad=delta_roles, estadio=.x,
                           color_by="roles", layout_brain=F))
dev.off()

# save plots (hemis) --------------------------------------------------------------------

delta_roles = 0.075
png("output/plots/redes_hemis_nobrain_"%+%str_remove(delta_roles,"\\.")%+%".png",
    width=1600, height=1200)
par(mfrow=c(2,2))
par(mai=c(0,0,0,2))
walk(estadios, ~grafo_plot(densidad=delta_roles, estadio=.x,
                           color_by="hemis", layout_brain=F))
dev.off()

# probar con manipulate ---------------------------------------------------

# dev.off()
# par(mai=c(0,0,0,0))
# library(manipulate)
# manipulate(
#   grafo_plot(
#     densidad=d, estadio=s, color_by=c, layout_brain=F, legends=F),
#   d = slider(0.025, 0.15, step=0.005, initial=0.075),
#   s = picker("W","N1","N2","N3"),
#   c = picker("area","hemis","roles","modulos"),
#   brain = picker(F,T)
# )
