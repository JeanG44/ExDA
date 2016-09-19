## A Function that returns summaries of varibles of three types(Numeric, Categorical, Time)

# Numeric: Numeric, Integer
# Categorical: character, factor
# Time: Date %Y-%d-%m (0000-00-00)

getSummary <- function(data, savefile=FALSE, filedir=NA, K=5, sepvalue=F){
  library(dplyr)
  ## functions that are used for summarization of data
  # 변수 타입 파악(로지컬일 경우 카테고리컬로 봄)
  classes <- sapply(data, class)
  classes <- ifelse(classes=='Date', 'Time',
                    ifelse(classes=='integer'|classes=='numeric', 'Numeric','Categorical'))
  class <- table(classes)
  class <- names(class)
  NumericExists <- ifelse('Numeric'%in%class, T, F)
  CategoricalExists <- ifelse('Categorical'%in%class, T, F)
  TimeExists <- ifelse('Time'%in%class, T, F)
  
  NumericVariables <- NA
  CategoricalVariables <- NA
  TimeVariables <- NA
  
  ##----------------------------------------------------숫자형 변수 요약
  if(NumericExists){
    
    # 결측값 개수를 반환하는 함수  
    MissingNum <- function(x){
      x <- as.character(x)
      missing <- sum(is.na(x))
      return(missing)
    }
    
    # 결측값의 비율을 반환하는 함수
    MissingRatio <- function(x){
      x <- as.character(x)
      missing <- sum(is.na(x))
      return(round(missing/length(x), 4))
    }
    
    # 요약 데이터 생성 함수 정의 
    getNumericSummary <- function(x){
      return(data.frame(Type='Numeric',  Missing=MissingNum(x), MissingRatio=MissingRatio(x),
                        Min=min(x, na.rm=TRUE), Max=max(x, na.rm=TRUE),
                        Mean=round(mean(x, na.rm=TRUE), 2), Median=median(x, na.rm=TRUE)))
    }
    
    # 요약 데이터 생성
    NumericVariables <- bind_rows(apply(select_(data,.dots=names(classes[classes=='Numeric'])), 2, getNumericSummary))%>%
      mutate(Name=names(classes[classes=='Numeric']),
             Nrow=nrow(data))%>%
      select(Name, Nrow, everything())
  }
    
##----------------------------------------------------범주형 변수 요약
  if(CategoricalExists){
    
    # 결측값 개수를 반환하는 함수  
    MissingNum <- function(x){
      x <- as.character(x)
      missing <- sum(is.na(x))
      return(missing)
    }
    
    # 결측값의 비율을 반환하는 함수
    MissingRatio <- function(x){
      x <- as.character(x)
      missing <- sum(is.na(x))
      return(round(missing/length(x), 4))
    }
    
    # 최빈값의 이름과 빈도를 반환하는 함수 정의
    Most <- function(x, sepvalue=F){
      x <- as.character(x)
      x <- x[!is.na(x)]
      x <- table(x)
      if(sepvalue){
        return(paste(names(x)[which.max(x)], max(x), sep=': '))
      }else{
        return(paste(names(x)[which.max(x)], max(x), sep=': '))# 나중에 수정해야할 부분
      }
    }
    # 최저 빈도를 나타내는 값의 이름과 빈도를 반환하는 함수 정의
    Least <- function(x, sepvalue=F){
      x <- as.character(x)
      x <- x[!is.na(x)]
      x <- table(x)
      if(sepvalue){
        return(paste(names(x)[which.min(x)], min(x), sep=': '))
      }else{
        return(paste(names(x)[which.min(x)], min(x), sep=': '))# 나중에 수정해야할 부분
      }
    }
    # 범주의 개수를 반환하는 함수 정의
    Numcategory <- function(x){
      x <- as.character(x)
      x <- x[!is.na(x)]
      return(length(unique(x)))
    }
    # 범주의 종류와 빈도를 반환하는 함수 정의
    Categories <- function(x){
      x <- as.character(x)
      x <- x[!is.na(x)]
      x <- table(x)
      x <- paste(paste(names(x), x, sep=': '), collapse=', ')
      return(x)
    }
    # 상위 K개 범주의 종류와 빈도를 반환하는 함수 정의
    TopK <- function(x, K=5){
      x <- as.character(x)
      x <- x[!is.na(x)]
      x <- table(x)
      K <- min(K, length(x))
      x <- x[order(x, decreasing=TRUE)]
      x <- x[1:K]
      x <- paste(paste(names(x), x, sep=': '), collapse=', ')
      return(x)
    }
    
    # 요약 데이터 생성 함수 정의 
    getCategoricalSummary <- function(x, K=5, sepvalue=F){
      return(data.frame(Type='Categorical',  Missing=MissingNum(x), MissingRatio=MissingRatio(x),
                        Least=Least(x, sepvalue=F), Most=Most(x, sepvalue=F),
                        NumCategory=Numcategory(x), TopK=TopK(x, K)))
    }
    
    # 요약 데이터 생성
    CategoricalVariables <- bind_rows(apply(select_(data,.dots=names(classes[classes=='Categorical'])), 2, getCategoricalSummary))%>%
      mutate(Name=names(classes[classes=='Categorical']),
             Nrow=nrow(data))%>%
      select(Name, Nrow, everything())
  }
##----------------------------------------------------시간 변수 요약
if(TimeExists){
  
  MissingRatioTime <- function(x){
    x <- as.Date(x)
    missing <- sum(is.na(x))
    return(round(missing/length(x), 4))
  }
  
  MissingNumTime <- function(x){
    x <- as.Date(x)
    missing <- sum(is.na(x))
    return(missing)
  }
  
  Earliest <- function(x){
    x <- as.Date(x)
    x <- x[!is.na(x)]
    return(min(x))
  }
  
  Latest <- function(x){
    x <- as.Date(x)
    x <- x[!is.na(x)]
    return(max(x))
  }
  
  Duration <- function(x){
    x <- as.Date(x)
    x <- x[!is.na(x)]
    d <- max(x)-min(x)
    return(paste(round(d, 2), 'days', sep=' '))
  }
  
  ## 요약 데이터 생성 함수 정의 
  getTimeSummary <- function(x){
    return(data.frame(Type='Time',  Missing=MissingNumTime(x), MissingRatio=MissingRatioTime(x),
                      Earliest=Earliest(x), Latest=Latest(x),
                      Duration=Duration(x)))
  }

  # 요약 데이터 생성
  TimeVariables <- bind_rows(apply(select_(data,.dots=names(classes[classes=='Time'])), 2, getTimeSummary))%>%
    mutate(Name=names(classes[classes=='Time']),
           Nrow=nrow(data))%>%
    select(Name, Nrow, everything())
}
  
  if(savefile){
    if(NumericExists){
      write.csv(NumericVariables, paste(filedir,'/Numeric.csv', sep=''), row.names=FALSE)
    }
    if(CategoricalExists){
      write.csv(CategoricalVariables, paste(filedir,'/Categorical.csv', sep=''), row.names=FALSE)
    }
    if(TimeExists){
      write.csv(TimeVariables, paste(filedir,'/Time.csv', sep=''), row.names=FALSE)
    }
  }
  
  return(list(Summary=list(
    NumericVariables=NumericVariables,
    CategoricalVariables=CategoricalVariables,
    TimeVariables=TimeVariables),
    Classes=classes
    ))
}
