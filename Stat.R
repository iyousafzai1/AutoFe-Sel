statistical <- function(...) {
  UseMethod("statistical")
}

#' @rdname statistical
#' @export
statistical.default <- function(x, y, features="all",
                                   summary=c("mean", "sd"), by.class=FALSE,
                                   transform=TRUE, ...) {
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
    features <- ls.statistical()
  }
  features <- match.arg(features, ls.statistical(), TRUE)
  colnames(x) <- make.names(colnames(x))
  
  if (length(summary) == 0) {
    summary <- "non.aggregated"
  }

  if (transform) {
    numdata <- binarize(x)
  } else {
    numdata <- x[, sapply(x, is.numeric), drop=FALSE]

    if (length(numdata) == 0) {
      if (by.class) {
        multiples <- c(ls.statistical.multiples(), 
                       setdiff(setdiff(ls.statistical(), 
                                       ls.statistical.multiples()), 
                               ls.statistical.exclude.byclass()))
      } else {
        multiples <- ls.statistical.multiples()
      }
      return(
        sapply(features, function(f) {
          post.processing(NA, summary, f %in% multiples, ...)
        }, simplify=FALSE)
      )
    }
  }
  
  y.num <- binarize(as.data.frame(y))
  x.cov <- stats::cov(numdata)
  
  extra <- list(
    y.num = y.num,
    cancor = tryCatch(
      stats::cancor(numdata, y.num),
      error=function(e) {
        warning(e)
        list(cor=c())
      }),
    x.cov = x.cov,
    eigenvalues = base::eigen(x.cov)
  )

  if(by.class) {
    exclude <- ls.statistical.exclude.byclass()
    measures <- sapply(levels(y), function(class) {
      new.data <- numdata[y==class, , drop=FALSE]
      new.x <- x[y==class, , drop=FALSE]
      nex.xcov <- stats::cov(new.data)
      new.extra <- list(
        x.cov = nex.xcov,
        eigenvalues = base::eigen(nex.xcov)
      )
      
      #new.data <- new.data[, apply(new.data, 2, stats::sd) != 0, drop=FALSE]
      aux <- sapply(features, function(f) {
        fn <- paste("m", f, sep=".")
        if (f %in% exclude) {
          do.call(fn, c(list(x=numdata, y=y, xorig=x, extra=extra), list(...)))
        } else {
          do.call(fn, c(list(x=new.data, xorig=new.x, extra=new.extra), list(...)))
        }
      }, simplify=FALSE)
      
      aux
    }, simplify=FALSE)
    
    sapply(features, function(f) {
      if (f %in% exclude) {
        values <- measures[[1]][[f]]
      } else {
        values <- lapply(measures, function(values) values[[f]])
      }
      
      post.processing(unlist(values), summary, 
                      !f %in% setdiff(exclude, ls.statistical.multiples()), ...)
    }, simplify=FALSE)
  } else {
    sapply(features, function(f) {
      fn <- paste("m", f, sep=".")
      measure <- do.call(fn, c(list(x=numdata, y=y, xorig=x, extra=extra), list(...)))
      post.processing(measure, summary, f %in% ls.statistical.multiples(), ...)
    }, simplify=FALSE)
  }
}

#' @rdname statistical
#' @export
statistical.formula <- function(formula, data, features="all",
                                   summary=c("mean", "sd"), by.class=FALSE,
                                   transform=TRUE, ...) {
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  statistical.default(modFrame[, -1], modFrame[, 1], features, summary,
                         by.class, transform, ...)
}

ls.statistical.exclude.byclass <- function() {
  c("canCor", "gravity", "nrDisc", "sdRatio", "wLambda")
}

m.canCor <- function(x, y, extra, ...) {
  if (length(extra$cancor$cor) == 0) return(NA)
  else return(extra$cancor$cor)
}

m.gravity <- function(x, y, ...) {
  classes <- table(y)
  minc <- which.min(classes)
  maxc <- which.max(classes[-minc])
  
  centers <- t(sapply(names(c(minc, maxc)), function(class){
    apply(x[y == class, , drop=FALSE], 2, base::mean)
  }))

  c(stats::dist(centers))
}

m.cor <- function(x, ...) {
  args <- list(...)
  method <- ifelse(is.null(args$method), "pearson", args$method)
  aux <- stats::cor(x, method=method)
  abs(aux[upper.tri(aux)])
}

m.cov <- function(x, y, extra, ...) {
  abs(extra$x.cov[upper.tri(extra$x.cov)])
}

m.nrDisc <- function(x, y, extra, ...) {
  length(extra$cancor$cor)
}

m.eigenvalues <- function(x, y, extra, ...) {
  extra$eigenvalues$values
}

m.gMean <- function(x, ...) {
  res1 <- apply(x, 2, prod)^(1/nrow(x))

  x[x < 1] <- NA
  res2 <- apply(x, 2, function(col) {
    exp(base::mean(log(col), ...))
  })

  coalesce(res1, res2)
}

m.hMean <- function(x, ...) {
  apply(x, 2, function(col) length(col) / sum(1/col))
}

m.iqRange <- function(x, ...) {
  apply(x, 2, stats::IQR)
}

m.kurtosis <- function(x, ...) {
  apply(x, 2, e1071::kurtosis)
}

m.mad <- function(x, ...) {
  apply(x, 2, stats::mad)
}

m.max <- function(x, ...) {
  apply(x, 2, base::max)
}

m.mean <- function(x, ...) {
  apply(x, 2, base::mean)
}

m.median <- function(x, ...) {
  apply(x, 2, stats::median)
}

m.min  <- function(x, ...) {
  apply(x, 2, base::min)
}

m.nrCorAttr <- function(x, ...) {
  sum(abs(m.cor(x, ...)) >= 0.5) / (ncol(x) * (ncol(x) - 1) / 2)
}

m.nrNorm <- function(x, ...) {
  sum(unlist(apply(x, 2, function(col) {
    p.value <- NA
    tryCatch(
      p.value <- stats::shapiro.test(col[seq(min(length(col), 5000))])$p.value, 
      error = function(e) e
    )
    p.value
  })) < 0.1, na.rm = TRUE)
}

m.nrOutliers <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  sum(apply(x, 2, function(x) {
    qs <- stats::quantile(x, na.rm=na.rm)
    iqr <- (qs[4] - qs[2]) * 1.5
    (qs[2] - iqr) > qs[1] | (qs[4] + iqr) < qs[5] 
  }))
}

m.range <- function(x, ...) {
  res <- apply(x, 2, base::range)
  res[2,] - res[1,]
}

m.sd <- function(x, ...) {
  args <- list(...)
  na.rm <- ifelse(is.null(args$na.rm), FALSE, args$na.rm)
  apply(x, 2, stats::sd, na.rm=na.rm)
}

m.sdRatio <- function(x, y, extra, ...) {
  p <- ncol(x)
  q <- nlevels(y)
  n <- length(y)
  ni <- table(y) - 1
  
  Si <- lapply(levels(y), function(class) stats::cov(x[y == class,, drop=FALSE]))
  S <- Reduce('+', mapply(function(Si, ni) ni*Si, S=Si, n=ni, SIMPLIFY=FALSE)) /
    (n - q)
  
  tryCatch({
    M <- (1 - ((2*p^2+3*p-1)/(6*(p+1)*(q-1))) * (sum(1/ni)-1/(n-q))) *
      ((n - q) * log(det(S)) - sum(ni * log(sapply(Si, det))))
    
    ifelse(is.na(M) | is.infinite(M), NA, exp(M / (p * sum(ni - 1))))
  }, warning = function(e) {
    NA
  })
}

m.skewness <- function(x, ...) {
  apply(x, 2, e1071::skewness)
}

m.sparsity <- function(xorig, ...) {
  (apply(xorig, 2, function(col) base::mean(table(col))) - 1) / (nrow(xorig) - 1)
}

m.tMean <- function(x, ...) {
  apply(x, 2, base::mean, trim=0.2)
}

m.var <- function(x, ...) {
  apply(x, 2, stats::var)
}

m.wLambda <- function(x, y, ...) {
  if (ncol(x) > 1) {
    as.numeric(rrcov::Wilks.test(x, grouping=y)$statistic)
  } else {
    return(NA)
  }
}
