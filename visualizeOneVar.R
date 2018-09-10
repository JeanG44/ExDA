## A functions that returns different visualizations of each variable depending on its data type.
## Now the result is only available in the way that the visualized results are saved as files.
## Available graphs
# numeric: boxplot
# categorical: histogram_all, histogram_topk
# time: Spectogram, WeekDaySummary
# 변수 타입별로 여러 그래프 한번에 그릴 수 있게 코드 수정 ex. 'histogram%in%categorical_graphs
visualizeOnevar <- function(data, save_file=TRUE, file_dir,
                            numeric_graph='boxplot',
                            skewed=TRUE,
                            categorical_graph='histogram_topk',
                            K=5,
                            time_graph='WeekdaySummary',
                            time_from=NA,
                            time_to=NA,
                            remove_outlier=TRUE,
                            outlier_rate=0.01){
  library(ggplot2)
  ## functions that are used for summarization of data
  classes <- sapply(data, class)
  classes <- ifelse(classes=='Date', 'Time',
                    ifelse(classes=='integer'|classes=='numeric', 'Numeric','Categorical'))
  classname <- table(classes)
  classname <- names(classname)
  NumericExists <- ifelse('Numeric'%in%classname, T, F)
  CategoricalExists <- ifelse('Categorical'%in%classname, T, F)
  TimeExists <- ifelse('Time'%in%classname, T, F)
  
  ##----------------------------------------------------visualize numerical variable
  if(NumericExists){
    if(numeric_graph=='boxplot'){
      if(skewed){
        x11(width=150, height=100, pointsize=20)
        par(oma=c(5,0,0,0))
        for(name in names(classes)[classes=='Numeric']){
          var <- data[[name]][!is.na(data[[name]])]
          outlier <- quantile(var, probs=seq(0,1,outlier_rate))
          var <- var[which(var>outlier[2]&var<outlier[(length(outlier)-1)])]
          var <- round(var, 5)
          ylabs <- quantile(var, probs=seq(0,1,0.1))
          boxplot(asinh(var), horizontal=TRUE, bixwex=7, axes=FALSE, frame.plot=TRUE,
                  width=150, height=50)
          axis(1, at=asinh(ylabs), labels=ylabs, las=2)
          title(name, font.main=24)
          savePlot(filename=paste(file_dir, '/boxplot_s_', name, '.png', sep=''), type=c('png'))
        }
      }else{
        x11(width=150, height=100, pointsize=20)
        for(name in names(classes)[classes=='Numeric']){
          var <- data[[name]][!is.na(data[[name]])]
          if(remove_outlier){
            outlier <- quantile(var, probs=seq(0,1,outlier_rate))
            var <- var[which(var>outlier[2]&var<outlier[(length(outlier)-1)])]
          }else{
          }
          var <- round(var, 5)
          boxplot(var, horizontal=TRUE, axes=TRUE, frame.plot=TRUE, width=150, height=50)
          title(name, font.main=24)
          savePlot(filename=paste(file_dir, '/boxplot_', name, '.png', sep=''), type=c('png'))
        }
      }
    }else{
    }
  }
  ##----------------------------------------------------범주형 변수 시각화
  if(CategoricalExists){
    if(categorical_graph=='histogram_all'){
      for(name in names(classes)[classes=='Categorical']){
        var <- data[[name]][!is.na(data[[name]])]
        var <- table(var)
        var <- var[order(var, decreasing=TRUE)]
        tmp <- as.data.frame(var)
        png(paste(file_dir, '/histogram_all_', name, '.png', sep=''),
            width=600, height=600, type='cairo', family='Dejavu Sans', pointsize=20)
        plot(ggplot(data=tmp, aes(x=var, y=Freq))+
               geom_bar(stat='identity')+
               ggtitle(name)+
               xlab('Category')+
               theme_bw()+
               theme(axis.text.x=element_blank(),
                     title=element_text(size=24)))
        dev.off()
      }
    }else if(categorical_graph=='histogram_topk'){
      for(name in names(classes)[classes=='Categorical']){
        var <- data[[name]][!is.na(data[[name]])]
        var <- table(var)
        var <- var[order(var, decreasing=TRUE)]
        k <- min(length(var), K)
        var <- var[1:k]
        tmp <- as.data.frame(var)
        tmp$var <- as.character(tmp$var)
        for(j in 1:length(tmp$var)){
          if(nchar(tmp$var[j])-20){
            tmp$var[j] <- paste(substr(tmp$var[j], 1,20), '...', sep='')
          }
        }
        tmp$var <- ordered(tmp$var, levels=tmp$var)
        png(paste(file_dir, '/histogram_top',k,'_', name, '.png', sep=''),
            width=600, height=600, type='cairo', family='Dejavu Sans', pointsize=20)
        plot(ggplot(data=tmp, aes(x=var, y=Freq))+
               geom_bar(stat='identity')+
               ggtitle(paste(name, 'Top_', k, 'Categories', sep=''))+
               xlab('Category')+
               theme_bw()+
               theme(axis.text.x=element_blank(),
                     title=element_text(size=24)))
        dev.off()
      }
    }else{
    }
  }
  ##----------------------------------------------------시간형 변수 시각화
  if(TimeExists){
    if(time_graph=='Spectogram'){
      library(reshape2)
      library(Cairo)
      library(dplyr)
      tmp <- select_(data,.dots=names(classes[classes=='Time']))
      tmp <- filter(tmp,complete.cases(tmp))
      tmp <- melt(tmp)
      time_from <- ifelse(is.na(time_from), as.character(min(tmp$value)), time_from)
      time_to <- ifelse(is.na(time_to), as.character(max(tmp$value)), time_to)
      timeStandard <- as.character(seq(as.Date(time_from), as.Date(time_to), by=1))
      rm(tmp)
      for(name in names(classes)[classes=='Time']){
        var <- data[[name]][!is.na(data[[name]])]
        var <- ordered(as.character(var), levels=timeStandard)
        var <- table(var)
        tmp <- as.data.frame(var)
        tmp$var <- as.Date(as.character(tmp$var))
        png(paste(file_dir, '/Spectogram_', name, '.png', sep=''),
            width=600, height=300, type='cairo', family='Dejavu Sans', pointsize=20)
        plot(ggplot(data=tmp, aes(x=var, y=Freq))+
               geom_line()+
               ggtitle(name)+
               xlab('Date')+
               theme_bw()+
               theme(axis.text.x=element_text(angle=90, hjust=1, size=11),
                     title=element_text(size=24)))
        dev.off()
      }
    }else if(time_graph=='WeekdaySummary'){
      for(name in names(classes)[classes=='Time']){
        var <- data[[name]][!is.na(data[[name]])]
        curr_locale <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME","English")
        var <- weekdays(var, abbreviate=TRUE)
        Sys.setlocale("LC_TIME",curr_locale)
        var <- toupper(var)
        var <- ordered(as.character(var), levels=c('MON','TUE','WED','THU','FRI','SAT','SUN'))
        var <- table(var)
        tmp <- as.data.frame(var)
        png(paste(file_dir, '/WeekdaySummary_', name, '.png', sep=''),
            width=600, height=600, type='cairo', family='Dejavu Sans', pointsize=20)
        plot(ggplot(data=tmp, aes(x=var, y=Freq))+
               geom_bar(stat='identity')+
               ggtitle(name)+
               xlab('Weekday')+
               theme_bw()+
               theme(axis.text.x=element_text(angle=90, hjust=1, size=11),
                     title=element_text(size=24)))
        dev.off()
      }
    }else{
    }
  }else{
  }
}