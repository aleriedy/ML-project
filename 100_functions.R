snp_fun <- function(PATH, P_Value) {
  x <- fread(PATH)
  ## specify the P-value threshold
  v1 <- which(x$P < P_Value)
  x_v1 <- x[v1, ]
  x_snp <- x_v1$SNP
  return(x_snp)
}


trainfun = function(data, target) {
  ## Normalize the data
  x <- normalizeFeatures(data,
                         target = target,
                         method = "standardize")
  
  nzv <- nearZeroVar(x)
  x <- x[,-nzv]
  
  df2 <- cor(x)
  hc <- findCorrelation(df2, cutoff = 0.90)
  hc <- sort(hc)
  x <- x[, -c(hc)]
  
  return(x)
} 