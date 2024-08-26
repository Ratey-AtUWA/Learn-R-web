# j <- 2; i <- 1
data0 <- ss2022[ ,colz]
for(j in 2:ncol(data0)){
  for(i in 1:nrow(data0)){
    # first check if guideline value or observation not missing!
    # cat("data0[i,j]",data0[i,j],"; DGV",ISQG[colnames(data0)[j],1],"; GV-high",ISQG[colnames(data0)[j],2],"\n")
    if(!is.na(data0[i,j]) & !is.na(ISQG[colnames(data0)[j],1]) & !is.na(ISQG[colnames(data0)[j],2])){
      # then add symbols to observations greater than or equal to guideline
      if(as.numeric(data0[i,j]) >= ISQG[colnames(data0)[j],2]) {
        data0[i,j] <- paste0(data0[i,j],"▲▲")
        # cat("i",i,"j",j,colnames(data0)[j],data0[i,j], colnames(ISQG)[1], ISQG[colnames(data0)[j],1], colnames(ISQG)[2], ISQG[colnames(data0)[j],2],"\n")
      } else if(as.numeric(data0[i,j]) < ISQG[colnames(data0)[j],2] & as.numeric(data0[i,j]) >= ISQG[colnames(data0)[j],1]) {
        data0[i,j] <- paste0(data0[i,j],"▲")
        # cat("i",i,"j",j,colnames(data0)[j],data0[i,j], colnames(ISQG)[1],ISQG[colnames(data0)[j],3], ISQG[colnames(data0)[j],1], colnames(ISQG)[2],ISQG[colnames(data0)[j],3], ISQG[colnames(data0)[j],2],"\n")
      }
    }
# i <- i+1
  }
}
# i <- 6 ; j <- 4
data0; str(data0)
