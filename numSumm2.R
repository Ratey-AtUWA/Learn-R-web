numSumm2 <- function(data){
  if(is.data.frame(data)){
    for(i in 1:NCOL(data)){
      if(!is.numeric(data[,i])) stop("All variables must be numeric")
    }
    summ0 <- data.frame(Variables=colnames(data),
                        mean=apply(data, 2, function(x){mean(x, na.rm=T)}),
                        sd = apply(data, 2, function(x){sd(x, na.rm=T)}),
                        min = apply(data, 2, function(x){min(x, na.rm=T)}),
                        median = apply(data, 2, function(x){median(x, na.rm=T)}),
                        max = apply(data, 2, function(x){max(x, na.rm=T)}),
                        n = apply(data, 2, function(x){length(na.omit(x))}),
                        nNA = apply(data, 2, function(x){sum(is.na(x))}))
  } else {
    if(!is.numeric(data)) stop("All variables must be numeric")
    summ0 <- data.frame(Variable = deparse(substitute(data)),
                        mean = mean(data, na.rm = T),
                        sd  =  sd(data, na.rm = T),
                        min  =  min(data, na.rm = T),
                        median  =  median(data, na.rm = T),
                        max  =  max(data, na.rm = T),
                        n  =  length(na.omit(data)),
                        nNA  =  sum(is.na(data)))
  }
  return(summ0)
}
