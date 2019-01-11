source("libraries.R")

# operador para concatenar texto:
"%+%" <- function(a,b) paste(a,b,sep="")

# matriz de adj no pesada en base a una pesado (fijando densidad)
rednp <- function(red, delta) {
  # numero de links que se conservan
  n = nrow(red) # cantidad de nodos
  maxlinks = n*(n-1)
  nlinks = (delta*maxlinks) %>% floor
  # grafo no pesado (quedan nlinks con link=1)
  diag(red) = 0 # ceros en diagonal
  umbral = sort(c(red),decreasing=TRUE)[nlinks] # umbral de conexion
  red_np = red>umbral
  red_np[red_np==T] = 1 # reemplaza por 0s y 1s
  return(red_np)
} 

# apply funcion binaria vectorizada(fn) 
# por columna/fila (marg) para dos matrices mat1 y mat2 de igual dim
mat_apply_bin = function(mat1, mat2, fn, marg=1) {
  res = list()
  if (marg==1) {
    for (i in 1:nrow(mat1)) {
      res[[i]] = fn(mat1[i,], mat2[i,])
    }
    out = Reduce(rbind, res)
  }
  if (marg==2) {
    for (j in 1:ncol(mat1)) {
      res[[j]] = fn(mat1[,j], mat2[,j])
    }
    out = Reduce(cbind, res)
  }
  dimnames(out)[[marg]] = NULL
  return(out)  
}


# transformacion minmax
minmax <- function(x) (x-min(x))/(max(x)-min(x))

# ALL duplicated indexes in a vector
every_dup = function(x) duplicated(x)|duplicated(x,fromLast=T)
