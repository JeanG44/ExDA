## A Function that searches if there are variables in hierarchical relationships
findHierarch <- function(data){
  library(Rcpp)
  library(dplyr)
  sourceCpp(code="
            #include <Rcpp.h> 
            using namespace Rcpp;
            // [[Rcpp::export]]
            NumericVector colMaxRcpp(NumericMatrix X) {
            int ncol = X.ncol();
            Rcpp::NumericVector out(ncol);
            for (int col = 0; col < ncol; col++){
            out[col]=Rcpp::max(X(_, col)); 
            } 
            return wrap(out);
            } ")

  getHpair <- function(pair, data){
    if(pair[1]==pair[2]){
      return(FALSE)
    }
    high <- data[[pair[1]]]
    low <- data[[pair[2]]]
    tmp <- table(high, low)
    return(all(colMaxRcpp(tmp)==colSums(tmp)))
  }
  data <- data%>%mutate_each(funs(as.character))
  pairs <- cbind(rep(vars, each=length(vars)), rep(vars, length(vars)))
  pairs <- split(pairs, 1:nrow(pairs))
  return(pairs[unlist(lapply(pairs, getHpair, data))])
}
