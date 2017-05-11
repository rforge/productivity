# Färe-Primont productivity index (fareprim)

fareprim <- function(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, 
  tech.change = TRUE, tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), 
  orientation = c("out", "in", "in-out"), cores = detectCores() - 1, scaled = FALSE, indices = FALSE, 
  by.id = NULL, by.year = NULL, out.levels = NULL, out.indices = NULL) {
  step1 <- check.1(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
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
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 1e-04)) 
      warning("Some quantity variables are not between 1e-4 and 1e5. We recommend rescaling the data 
or set the scaled option to TRUE to avoid numerical problems", call. = FALSE)
  } else {
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)], 
      2, FUN = function(x) x/mean(x))
  }
  XREFs <- t(as.matrix(data[, step1$x.vars]))
  YREFs <- t(as.matrix(data[, step1$y.vars]))
  P.fp <- du.fpo(Xs = apply(XREFs, 1, mean), Ys = apply(YREFs, 1, mean), XREFs, YREFs, rts)
  W.fp <- du.fpi(Xs = apply(XREFs, 1, mean), Ys = apply(YREFs, 1, mean), XREFs, YREFs, rts)
    if (cores <= 1) {
      cores <- 1
    }
  registerDoParallel(cores = cores)
  if (tech.change == TRUE) {
    res.fp.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", 
      "doParallel"), .export = c("fp.1", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% 
      {
        fp.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, PRICESO = P.fp, PRICESI = W.fp)
      }
  } else {
    res.fp.loop <- fp.2(data, data.in, step1, rts, orientation, PRICESO = P.fp, PRICESI = W.fp)
  }
  registerDoSEQ()
  stopImplicitCluster()
  row.names(res.fp.loop) <- seq(1:dim(res.fp.loop)[1])
  fp.data <- data[, c(step1$id.var, step1$time.var)]
  fp.data <- cbind(fp.data, res.fp.loop)
  if (!is.null(out.levels)) 
    write.csv(fp.data, file = out.levels)
  if (indices == FALSE) {
    res.tfp <- list(Levels = fp.data)
    class(res.tfp) <- c("list", "FarePrimont")
    return(res.tfp)
  } else {
    names.fp <- c("REV", "COST", "PROF", "P", "W", "TT", "AO", "AI", "TFP", "MP", "TFPE", 
      "OTE", "OSE", "OME", "ROSE", "OSME", "ITE", "ISE", "IME", "RISE", "ISME", "RME", 
      "OTE.ITE", "OSE.ISE", "OME.IME", "ROSE.RISE", "OSME.ISME")
    names.var <- names(fp.data)
    names.ind <- names.var[names.var %in% names.fp]
    registerDoParallel(cores = cores)
    if (!(is.null(by.id)) & !(is.null(by.year))) {
      id.vec <- unique(fp.data[, 1])
      if (by.id > length(id.vec)) 
        stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
      if (by.year > length(id.vec)) 
        stop("by.year is out of range: by.year must be  lower or equal to ", paste(length(year.vec)), 
          .call = FALSE)
      indices.1 <- fp.data[, names.ind]/matrix(fp.data[fp.data[, 2] == year.vec[by.year], 
        names.ind][by.id, ], nrow = 1)
    } else {
      if (!(is.null(by.id)) & (is.null(by.year))) {
        if (by.id > length(id.vec)) 
          stop("by.id is out of range: by.id must be lower or equal to ", paste(length(id.vec)), 
          .call = FALSE)
        indices.1 <- fp.data[, names.ind]/matrix(fp.data[by.id, names.ind], nrow = 1)
      } else {
        if ((is.null(by.id)) & !(is.null(by.year))) {
          if (by.year > length(id.vec)) 
          stop("by.year is out of range: by.year must be lower or equal to ", paste(length(year.vec)), 
            .call = FALSE)
          indices.1 <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            fp.data[fp.data[, 2] == ano, names.ind]/fp.data[fp.data[, 2] == year.vec[by.year], 
            names.ind]
          }
        } else {
          indices.1 <- foreach(ano = year.vec, .combine = rbind, .packages = c("doParallel")) %dopar% 
          {
            fp.data[fp.data[, 2] == ano, names.ind]/fp.data[fp.data[, 2] == year.vec[1], 
            names.ind]
          }
        }
      }
    }
    registerDoSEQ()
    stopImplicitCluster()
    
    indices.2 <- fp.data[, 1:2]
    indices.2 <- cbind(indices.2, indices.1)
    names(indices.2)[-c(1, 2)] <- paste0("d", names.ind)
    if (!is.null(out.indices)) 
      write.csv(indices.2, file = out.indices)
    res.tfp <- list(Levels = fp.data, Indices = indices.2)
    class(res.tfp) <- c("list", "FarePrimont")
    return(res.tfp)
  }
}
