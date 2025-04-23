#### Updated function to sort the qlist object  ####
## author: pophelper package version 2.3.1, function sortQ
## adapted by Florian Kunz 23.04.2025

## Function needed update, as apparently the inbuilt order() function was reworked in newer R and didnt work anymore within sortQ
## In line 41, the order statement was updated to work properly

## Minor update in code necessary, so full credits to original package pophelper, version 2.3.1; please refer to documentation of sortQ

sortQ_update <- function(qlist, by = "k", decreasing = FALSE, debug = FALSE){
  is.qlist(qlist)
  if (length(by) == 0) 
    stop("sortQ: Argument 'by' must not be length zero.")
  if (!is.character(by)) 
    stop("sortQ: Argument 'by' must be a character.")
  if (!is.logical(decreasing)) 
    stop("sortQ: Argument 'decreasing' must be a logical datatype.")
  fun1 <- function(x) as.matrix(unlist(attributes(x)))
  a <- lapply(qlist, fun1)
  if (debug) 
    print(a)
  if (any(!sapply(a, function(x) any(grepl(paste0(by, collapse = "|"), 
                                           rownames(x)))))) {
    stop(paste0("One or more of the attributes provided in by (", 
                by, ") is missing in one or more runs. If 'ind' or 'k' is missing, use 'as.qlist()' to add them."))
  }
  b <- as.data.frame(t(as.data.frame(lapply(a, function(x, 
                                                        y) x[y, ], by), stringAsFactors = FALSE)), stringsAsFactors = FALSE)
  fun2 <- function(x) if (all(!is.na(as.numeric(as.character(x))))) {
    return(as.numeric(as.character(x)))
  }
  else {
    return(x)
  }
  b <- as.data.frame(sapply(b, fun2), stringAsFactors = FALSE)
  if (debug) {
    print(str(b))
    print(b)
  }
  if (length(by) == 1) 
    ord <- order(b[[by]]) # adapted line, original line (ord <- order(b[, by, drop = FALSE])) didnt work
  if (length(by) > 1) 
    ord <- do.call(order, b[, by, drop = FALSE])
  if (decreasing) 
    ord <- rev(ord)
  return(qlist[ord])
}
