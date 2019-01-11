
# intro -------------------------------------------------------------------

source("libraries.R")
source("functions.R")

# data --------------------------------------------------------------------

estadios = c("N1","N2","N3","W")
# red promedio de cada estadio
redes = readRDS("data/working/redes_promedio.RDS")

# corrplot ----------------------------------------------------------------
# create and save
for (e in estadios) {
  png("output/plots/corrplot_"%+%e%+%".png", width=600, height=600)
  corrplot::corrplot(redes[[e]], is.corr=TRUE, tl.pos=F,
                     bg="transparent", cl.cex=1.5)
  title(e, adj=0, cex.main=2.5, line=2)
  # segments(x0=seq(0,117,length.out=5), y0=rep(0,5), 
  # x1=seq(0,117,length.out=5), y1=rep(117,5), lwd=2)
  # text(x=seq(0,117,length.out=5), y=rep(0,5), labels="aa")
  # segments(c(0.5, 0.5), c(0.5, 5.5), c(8.5, 8.5), c(0.5,5.5), lwd=3)
  dev.off()
}