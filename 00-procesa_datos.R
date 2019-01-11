# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# data --------------------------------------------------------------------

path = "data/raw/sujetos/"
files = list.files(path, full.names=T)
estadios = c("N1","N2","N3","W")
# matrices originales para cada estadio
bases = list()
for (e in estadios) {
  fs = files %>% "["(str_detect(.,e%+%"_")) 
  sujs = str_match(fs, "_(.+)\\.csv")[,2]
  bases[[e]] = map(fs, ~read.csv(.x, header=F) %>% as.matrix) %>% setNames(sujs)
}
# matrix promedio de cada estadio
bases_mean = map(bases, ~Reduce("+",.x)/length(.x))
# datos sobre las regiones
regiones <- read_csv("data/raw/aal_extended.csv", col_names=F,
                     col_types=cols(.default="c")) %>% 
  select(-1) %>% setNames(c("region","id","area","hemis"))

# save --------------------------------------------------------------------

# bases originales (lista)
saveRDS(bases, "data/working/redes_individuales.RDS")
# bases promedio (lista)
saveRDS(bases_mean, "data/working/redes_promedio.RDS")
# datos sobre las regiones
saveRDS(regiones, file="data/working/regiones_data.RDS")
