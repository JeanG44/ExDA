crossTable <- function(x,y, xlabel=NULL, ylabel=NULL, decimal=NA, save_csv=F, filedir=getwd()){
  if(!is.null(xlabel)){
    x <- as.character(x)
    x <- ordered(x, levels=xlabel)
  }
  if(!is.null(ylabel)){
    y <- as.character(y)
    y <- ordered(y, levels=ylabel)
  }
  
  tmp <- as.data.frame(table(x,y))
  tmp <- dcast(tmp, x~y, value.var='Freq', sum)
  row.names(tmp) <- tmp[,1]
  tmp <- tmp[,2:3]
  tmp[,ncol(tmp)+1] <- rowSums(tmp)
  tmp[nrow(tmp)+1,] <- colSums(tmp)
  row.names(tmp)[nrow(tmp)] <- 'sum'
  names(tmp)[ncol(tmp)] <- 'sum'
  
  tables <- list()
  tables$frequency <- tmp
  if(is.na(decimal)){
    tables$total_prop <- tmp/tmp[nrow(tmp),ncol(tmp)]
    tables$row_prop <- as.matrix(tmp)/tmp[,ncol(tmp)]
    tables$column_prop <- t(t(as.matrix(tmp))/as.numeric(tmp[nrow(tmp),]))
  }else{
    tables$total_prop <- round(tmp/tmp[nrow(tmp),ncol(tmp)],decimal)
    tables$row_prop <- round(as.matrix(tmp)/tmp[,ncol(tmp)],decimal)
    tables$column_prop <- round(t(t(as.matrix(tmp))/as.numeric(tmp[nrow(tmp),])),decimal)
  }
  if(save_csv){
    for(i in names(tables)){
      write.csv(tables[[i]], paste(filedir,'/', i, '.csv', sep=''))
    }
  }
  return(tables)
}




r <- crossTable(x,y, decimal=4)
