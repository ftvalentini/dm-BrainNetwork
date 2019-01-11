# Comparacion modularidad-nclusters entre grupos (estadios)
# genera plots con modularidad-nclusters promedio por delta con datos individuales

# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# read data ----------------------------------------------------------------

# objeto con info de redes individuales por sujeto por estadio por delta
indiv = readRDS(file="data/working/analisis_individuales.RDS")

# datos -------------------------------------------------------------------

# variables a graficar
vars = c("hubs","provincialhubs","provincialnodes","connectornodes","modularidad","nclusters") 
# anovas y pvalores (Tukey) por delta para cada variable
nest_delta = indiv %>% 
  group_by(delta) %>%
  nest() 
comps = nest_delta
for (v in vars) {
  comps[["aov_"%+%v]] = map(comps$data,
                            ~nlme::lme(as.formula(v%+%"~state"), random=~1|sujeto, data=.x))
  comps[["tuk_"%+%v]] = map(comps[["aov_"%+%v]], 
                            ~multcomp::glht(.x, linfct=multcomp::mcp(state="Tukey")))
  comps[["p_"%+%v]] = map(comps[["tuk_"%+%v]], ~tidy(summary(.x)) %$% p.value)
}
# pvalores extraidos para poner en plots
pvals_comp = comps %>% unnest(!!!syms("p_"%+%vars)) %>% 
  mutate(
    # repite nombre de comparaciones en una var nueva para identificarlas
    contraste = comps[["tuk_"%+%vars[1]]][[1]]$linfct %>% rownames() %>% rep(nrow(comps))
  ) %>%
  # solo contrastes que incluyen a W
  dplyr::filter(str_detect(contraste, "W"))
# pvalores ajustados por FDR para cada contraste
for (v in vars) {
  # crea vble nueva de padj
  pvals_comp[['padj_'%+%v]] = NA
  for (c in unique(pvals_comp$contraste)) {
    # extrae pvals originales
    p_temp = pvals_comp %>% dplyr::filter(contraste==c) %>% "[["("p_"%+%v)
    # crea y guarda pvals transformados en dataframe
    pvals_comp[pvals_comp$contraste==c, 'padj_'%+%v] = p_temp %>% p.adjust(method="fdr")
  }
  # variables de asteriscos en padj<0.05
  pvals_comp[[v]] = if_else(pvals_comp[["padj_"%+%v]]<0.05, "*", "")
}

# plots -------------------------------------------------------------------

# graficos de comparacion de medias en lista (con loop porque ya fue)
gcomp = list(N1=NULL, N2=NULL, N3=NULL)
for (e in c("N1","N2","N3")) {
  for (v in vars) {
    # datos (temporales)
    res = indiv %>% 
      group_by(delta, state) %>% 
      summarise(media = mean(get(v)),
                sem = sd(get(v))/sqrt(n()))
    # asteriscos (temporales)
    ast = pvals_comp %>% dplyr::filter(contraste==e%+%" - W") %>% 
      select(delta, v)
    # graficos
    gcomp[[e]][[v]] = ggplot(res %>% dplyr::filter(state %in% c("W",e)),
                             aes(x=delta, y=media, color=state, group=state)) +
      geom_line(size=1.5) +
      geom_point() +
      geom_errorbar(aes(ymin=media-sem, ymax=media+sem), width=.0005) +
      xlab("Densidad") +
      ylab(NULL) +
      labs(color="Estados") +
      theme_minimal() +
      theme(text=element_text(size=35), axis.title.x=element_text(size=25), 
            legend.position="bottom") +
      # agrega asteriscos de signif a los graficos
      annotate("text", x=ast$delta, y=0.3, label=ast[[v]], cex=15)
    NULL
  }
}

# save plots --------------------------------------------------------------

titles = c("Hubs","Provincial hubs","Provincial nodes","Connector nodes",
           "Modularidad","Número de clusters") %>% setNames(vars)
# guarda grids por variable
walk(vars,
     ~ggsave(filename="output/plots/comp_"%+%.x%+%".png",
             plot=cowplot::plot_grid(gcomp$N1[[.x]],
                                     gcomp$N2[[.x]],
                                     gcomp$N3[[.x]], 
                                     ncol=3, 
                                     labels=c(titles[.x],NULL,NULL), label_size=40),
             width=30, height=9, units="in", dpi=100))



# 
# # lista no anidada y con nombres para poder guardar
# gcomp_save = gcomp %>% unlist(recursive=F) %>% 
#   setNames(str_replace(names(.), "\\." ,"_"))
# # save
# walk(names(gcomp_save),
#      ~ggsave(filename="output/plots/comp_"%+%.x%+%".png", plot=gcomp_save[[.x]],
#              width=10, height=6, units="in", dpi=100))