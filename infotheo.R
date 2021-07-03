
infotheo <- function(...) {
  UseMethod("infotheo")
}

#' @rdname infotheo
#' @export
infotheo.default <- function(x, y, features="all", summary=c("mean", "sd"),
                                transform=TRUE, ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(features[1] == "all") {
    features <- ls.infotheo()
  }
  features <- match.arg(features, ls.infotheo(), TRUE)
  colnames(x) <- make.names(colnames(x))
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }
  
  if (transform) {
    x.dis <- categorize(x)
  } else {
    x.dis <- x[, !sapply(x, is.numeric), drop=FALSE]
    
    if (length(x.dis) == 0) {
      return(
        sapply(features, function(f) {
          post.processing(NA, summary, f %in% ls.infotheo.multiples(), ...)
        }, simplify=FALSE)
      )
    }
  }
  
  #Remove constant attributes
  x.dis <- x.dis[, sapply(x.dis, nlevels) > 1, drop=FALSE]
  
  extra <- list(
    y.entropy = entropy(y),
    y.log = base::log2(nlevels(y)),
    x.entropy = sapply(x.dis, entropy),
    x.log = sapply(sapply(x.dis, nlevels), base::log2),
    mutinf = sapply(x.dis, mutinf, y=y)
  )

  sapply(features, function(f) {
    fn <- paste("m", f, sep=".")
    measure <- do.call(fn, c(list(x=x.dis, y=y, extra=extra), list(...)))
    post.processing(measure, summary, f %in% ls.infotheo.multiples(), ...)
  }, simplify=FALSE)
}

#' @rdname infotheo
#' @export
infotheo.formula <- function(formula, data, features="all",
                                summary=c("mean", "sd"), 
                                transform=TRUE, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  infotheo.default(modFrame[, -1], modFrame[, 1], features, summary, 
                      transform, ...)
}

#' List the information theoretical meta-features
#'
#' @return A list of information theoretical meta-features names
#' @export
#'
#' @examples
#' ls.infotheo()
ls.infotheo <- function() {
  c("attrConc", "attrEnt", "classConc", "classEnt", "eqNumAttr", "jointEnt", 
    "mutInf", "nsRatio")
}

ls.infotheo.multiples <- function() {
  c("attrConc", "attrEnt", "classConc", "jointEnt", "mutInf")
}

m.attrConc <- function(x, ...) {
  if (ncol(x) == 1) return(NA)
  comb <- expand.grid(i=seq(ncol(x)), j=seq(ncol(x)))
  comb <- comb[comb$i != comb$j, ]

  mapply(function(i, j) {
    concentration.coefficient(x[, i], x[, j])
  }, i=comb$i, j=comb$j)
}

m.attrEnt <- function(extra, ...) {
  extra$x.entropy
}

m.classConc <- function(x, y, ...) {
  apply(x, 2, concentration.coefficient, y)
}

m.classEnt <- function(extra, ...) {
  extra$y.entropy
}

m.eqNumAttr <- function(extra, ...) {
  extra$y.entropy / mean(extra$mutinf)
}

m.jointEnt <- function(x, y, ...) {
  joint.data <- sapply(as.data.frame(sapply(x, paste, y)), as.factor)
  sapply(as.data.frame(joint.data), entropy)
}

m.mutInf <- function(extra, ...) {
  extra$mutinf
}

m.nsRatio <- function(extra, ...) {
  mutinf <- mean(extra$mutinf)
  (mean(extra$x.entropy) - mutinf) / mutinf
}

mutinf <- function(x, y) {
  entropy(x) + entropy(y) - entropy(paste(x, y))
}
