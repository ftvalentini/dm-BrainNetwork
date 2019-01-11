# Plots con medidas de resumen por delta en base a red promedio

# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# read data ----------------------------------------------------------------

# objeto con info de red promedio por estadio por delta
prom = readRDS(file="data/working/analisis_promedio.RDS")

# plots --------------------------------------------------------------------
# nombres de las variables de cada plot
vars = c("grado","cercania","autovec","intermed","diametro","clustering",
         "modularidad","nclusters","modularidad_g_rand_eqd",
         "nclusters_g_rand_eqd")
# nombre del titulo de cada plot
titles = c("Grado promedio","Cercanía promedio","Centralidad de autovector promedio",
           "Intermediación promedio","Diámetro","Coeficiente de clustering",
           "Coeficiente de modularidad","Número de comunidades",
           "Coeficiente de modularidad (red aleatoria)",
           "Número de comunidades (red aleatoria)") %>% setNames(vars)
# lista de plots
gresumen = vars %>% map(
  ~ggplot(prom) +
    geom_line(aes_string(x="delta", y=.x, color="state"), size=1.5) +
    xlab("Densidad") +
    ylab(NULL) +
    labs(color="Estados", title=titles[.x]) +
    theme_minimal() +
    theme(text=element_text(size=25), axis.title.x=element_text(size=20)) +
    NULL
) %>% setNames(vars)

# agrega plots indice RAND vs W (1 plot para cada estadio para poner astericos)
for (e in c("N1","N2","N3")) {
  # asteriscos (temporales)
  signif = prom %>% dplyr::filter(state==e) %>%
    mutate(ast = if_else(pval_rand<0.05, "*", "")) %>% select(delta, ast)
  gresumen[["rand"%+%e]] = ggplot(prom %>% dplyr::filter(state %in% e),
                                  aes(x=delta, y=rand_w)) +
    geom_line(size=1.5) +
    xlab("Densidad") +
    ylab(NULL) +
    ylim(c(0.3, 0.95)) +
    theme_minimal() +
    labs(title=e) + 
    # agrega asteriscos de signif a los graficos
    annotate("text", x=signif$delta, y=0.3, label=signif$ast, cex=15) +
    theme(text=element_text(size=35), axis.title.x=element_text(size=25)) +
    NULL
}
# ajustes para plots particulares
gresumen$modularidad = gresumen$modularidad + 
  ylim(c(0.3, 1))
gresumen$modularidad_g_rand_eqd = gresumen$modularidad_g_rand_eqd +
  ylim(c(0.3, 1))
gresumen$nclusters = gresumen$nclusters +
  ylim(c(0, 45))
gresumen$nclusters_g_rand_eqd = gresumen$nclusters_g_rand_eqd +
  ylim(c(0, 45))

# save plots --------------------------------------------------------------

walk2(gresumen, names(gresumen), ~ggsave(filename="output/plots/resumen_"%+%.y%+%".png",
                                         plot=.x,
                                         width=10, height=6, units="in", dpi=100))
