## Lowe productivity index.

lowe <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), 
  cores = detectCores() - 1, scaled = FALSE, indices = FALSE, by.id = NULL, by.year = NULL, out.levels = NULL, 
  out.indices = NULL) {
  step1 <- check.2(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS)) 
    stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in", "in-out")
  if (!(orientation %in% ORIENTATION)) 
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  data.in <- data[, c(step1$time.var, step1$y.vars, step1$x.vars)]
  year.vec <- unique(data[, time.var])
  if (scaled == FALSE) {
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 
      1e-04)) 
      warning("Some quantity variables are not between 1e-4 and 1e5. We recommend rescaling the data 
or set the scaled option to TRUE to avoid numerical problems", 
        call. = FALSE)
  } else {
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)], 
      2, FUN = function(x) x/mean(x))
  }
  P.lo <- apply(as.matrix(data[, step1$p.vars]), 2, mean)
  names(P.lo) <- paste0("U", 1:length(step1$p.vars))
  W.lo <- apply(as.matrix(data[, step1$w.vars]), 2, mean)
  names(W.lo) <- paste0("V", 1:length(step1$w.vars))
    if (cores <= 1) {
      cores <- 1
    }
  registerDoParallel(cores = cores)
  if (tech.change == TRUE) {
    res.lo.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", 
      "doParallel"), .export = c("lo.1", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% 
      {
        lo.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, PRICESO = P.lo, PRICESI = W.lo)
      }
  } else {
    res.lo.loop <- lo.2(data, data.in, step1, rts, orientation, PRICESO = P.lo, PRICESI = W.lo)
  }
  registerDoSEQ()
  stopImplicitCluster()
  
  row.names(res.lo.loop) <- seq(1:dim(res.lo.loop)[1])
  lo.data <- data[, c(step1$id.var, step1$time.var)]
  lo.data <- cbind(lo.data, res.lo.loop)
  if (!is.null(out.levels)) 
    write.csv(lo.data, file = out.levels)
  if (indices == FALSE) {
    res.tfp <- list(Levels = lo.data)
    class(res.tfp) <- c("list", "Lowe")
    return(res.tfp)
  } else {
    registerDoParallel(cores = cores)
    if (!(is.null(by.id)) & !(is.null(by.year))) {
      id.vec <- unique(lo.data[, 1])
      if (by.id > length(id.vec)) 
        stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
      if (by.year > length(id.vec)) 
        stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)), 
          .call = FALSE)
      indices.1 <- lo.data[, 3:19]/matrix(lo.data[lo.data[, 2] == year.vec[by.year], 
        3:19][by.id, ], nrow = 1)
    } else {
      if (!(is.null(by.id)) & (is.null(by.year))) {
        if (by.id > length(id.vec)) 
          stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
        indices.1 <- lo.data[, 3:19]/matrix(lo.data[by.id, 3:19], nrow = 1)
      } else {
        if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec)) 
          stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)), 
            .call = FALSE)
          indices.1 <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            lo.data[lo.data[, 2] == ano, 3:19]/lo.data[lo.data[, 2] == year.vec[by.year], 
            3:19]
          }
        } else {
          indices.1 <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            lo.data[lo.data[, 2] == ano, 3:19]/lo.data[lo.data[, 2] == year.vec[1], 
            3:19]
          }
        }
      }
    }
    registerDoSEQ()
    stopImplicitCluster()
    
    indices.2 <- lo.data[, 1:2]
    indices.2 <- cbind(indices.2, indices.1)
    names(indices.2)[3:19] <- paste0("d", names(lo.data[, 3:19]))
    if (!is.null(out.indices)) 
      write.csv(indices.2, file = out.indices)
    res.tfp <- list(Levels = lo.data, Indices = indices.2)
    class(res.tfp) <- c("list", "Lowe")
    return(res.tfp)
  }
}
