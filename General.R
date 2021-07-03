
general.default <- function(x, y, features="all", summary=c("mean", "sd"), 
                               ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all") {
    features <- ls.general()
  }
  features <- match.arg(features, ls.general(), TRUE)
  colnames(x) <- make.names(colnames(x))
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, c(list(x=x, y=y), list(...)))
    post.processing(measure, summary, f %in% ls.general.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname general
#' @export
general.formula <- function(formula, data, features="all", 
                               summary=c("mean", "sd"), ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  general.default(modFrame[, -1], modFrame[, 1], features, summary, ...)
}

#' List the general meta-features
#'
#' @return A list of general meta-features names
#' @export
#'
#' @examples
#' ls.general()
ls.general <- function() {
  c("attrToInst", "catToNum", "freqClass", "instToAttr", "nrAttr", "nrBin", 
    "nrCat", "nrClass", "nrInst", "nrNum",  "numToCat")
}

ls.general.multiples <- function() {
  c("freqClass")
}

#Meta-features
m.attrToInst <- function(x, ...) {
  m.nrAttr(x) / m.nrInst(x)
}

m.catToNum <- function(x, ...) {
  nnum <- m.nrNum(x)
  if (nnum == 0) return(NA)
  m.nrCat(x) / nnum
}

m.freqClass <- function(y, ...) {
  as.numeric(table(y)) / length(y)
}

m.instToAttr <- function(x, ...) {
  m.nrInst(x) / m.nrAttr(x)
}

m.nrAttr <- function(x, ...) {
  ncol(x)
}

m.nrBin <- function(x, ...) {
  sum(apply(x, 2, function (col) length(table(col)) == 2))
}

m.nrCat <- function(x, ...) {
  m.nrAttr(x) - m.nrNum(x)
}

m.nrClass <- function(y, ...) {
  length(unique(y))
}

m.nrInst <- function(x, ...) {
  nrow(x)
}

m.nrNum <- function(x, ...) {
  sum(sapply(x, is.numeric))
}

m.numToCat <- function(x, ...) {
  ncat <- m.nrCat(x)
  if (ncat == 0) return(NA)
  m.nrNum(x) / ncat
}
