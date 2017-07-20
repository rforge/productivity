## Lowe productivity index.

lowe <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), 
  parallel = FALSE, cores = max(1, detectCores() - 1), scaled = FALSE, by.id = NULL, by.year = NULL) {
  step1 <- check.2(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS)) 
    stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in", "in-out")
  if (!(orientation %in% ORIENTATION)) 
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  data <- data[order(data[, step1$time.var], data[, step1$id.var]), ]
  year.vec <- unique(data[, time.var])
  if (scaled == FALSE) {
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 
      1e-04)) 
      warning("Some quantity variables are not between 1e-4 and 1e5. We recommend rescaling the data 
or set the scaled option to TRUE to avoid numerical problems\n\r", 
        call. = FALSE)
  } else {
    data.in <- data[, c(step1$time.var, step1$y.vars, step1$x.vars)]
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)], 
      2, FUN = function(x) x/mean(x))
  }
  P.lo <- apply(as.matrix(data[, step1$p.vars]), 2, mean)
  names(P.lo) <- paste0("U", 1:length(step1$p.vars))
  W.lo <- apply(as.matrix(data[, step1$w.vars]), 2, mean)
  names(W.lo) <- paste0("V", 1:length(step1$w.vars))
  if (parallel == TRUE) {
    if (cores == 1) {
      registerDoSEQ()
    } else {
      registerDoParallel(cores = cores)
    }
  } else {
    registerDoSEQ()
  }
  if (tech.change == TRUE) {
    res.lo.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", 
      "doParallel"), .export = c("lo.1", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp", 
                                 "DO.shdu", "DI.shdu")) %dopar% 
      {
        lo.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled,
             PRICESO = P.lo, PRICESI = W.lo)
      }
  } else {
    res.lo.loop <- lo.2(data, data.in, step1, rts, orientation, parallel, scaled,
                        PRICESO = P.lo, PRICESI = W.lo)
  }
  registerDoSEQ()
  stopImplicitCluster()
  
  res.lo.loop <- round(res.lo.loop, 4)
  row.names(res.lo.loop) <- seq(1:dim(res.lo.loop)[1])
  lo.data <- data[, c(step1$id.var, step1$time.var)]
  lo.data <- cbind(lo.data, res.lo.loop[, 1:18])
    if (parallel == TRUE) {
      if (cores == 1) {
        registerDoSEQ()
      } else {
        registerDoParallel(cores = cores)
      }
    } else {
      registerDoSEQ()
    }
    id.vec <- unique(lo.data[, 1])
    if (!(is.null(by.id)) & !(is.null(by.year))) {
      if (by.id > length(id.vec)) 
        stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
      if (by.year > length(id.vec)) 
        stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)), 
          .call = FALSE)
      indices.1 <- lo.data[, 3:20]/matrix(lo.data[lo.data[, 2] == year.vec[by.year], 
        3:20][by.id, ], nrow = 1)
    } else {
      if (!(is.null(by.id)) & (is.null(by.year))) {
        if (by.id > length(id.vec)) 
          stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
        indices.1 <- lo.data[, 3:20]/matrix(lo.data[by.id, 3:20], nrow = 1)
      } else {
        if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec)) 
          stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)), 
            .call = FALSE)
          indices.1 <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            lo.data[lo.data[, 2] == ano, 3:20]/lo.data[lo.data[, 2] == year.vec[by.year], 
            3:19]
          }
        } else {
          indices.1 <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            lo.data[lo.data[, 2] == ano, 3:20]/lo.data[lo.data[, 2] == year.vec[1], 
            3:20]
          }
        }
      }
    }
    registerDoSEQ()
    stopImplicitCluster()
    
    indices.2 <- lo.data[, 1:2]
    indices.2 <- cbind(indices.2, indices.1)
    names(indices.2)[3:20] <- paste0("d", names(lo.data[, 3:20]))
    Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)], res.lo.loop[, 19:(dim(res.lo.loop)[2])])
    names(Shadowp) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
      res.tfp <- list(Levels = lo.data, Changes = indices.2, Shadowp = Shadowp)
    class(res.tfp) <- c("list", "Lowe")
    return(res.tfp)
}
