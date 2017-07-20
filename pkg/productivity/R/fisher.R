# Fisher productivity index (fisher)

fisher <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), 
  parallel = FALSE, cores = max(1, detectCores() - 1), scaled = FALSE) {
  step1 <- check.2(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!is.pbalanced(x = data, index = c(id.var, time.var))) 
    stop("Fisher index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
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
    res.fish.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", 
      "doParallel"), .export = c("fish.1", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp", 
      "DO.shdu", "DI.shdu")) %dopar% {
      fish.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled)
    }
  } else {
    res.fish.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", 
      "doParallel"), .export = c("fish.2", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp", 
      "DO.shdu", "DI.shdu")) %dopar% {
      fish.2(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled)
    }
  }
  registerDoSEQ()
  stopImplicitCluster()
  
  res.fish.loop <- round(res.fish.loop, 4)
  row.names(res.fish.loop) <- seq(1:dim(res.fish.loop)[1])
  fish.data <- data[, c(step1$id.var, step1$time.var)]
  fish.data <- cbind(fish.data, res.fish.loop[, 1:18])

    res.fish.loop2 <- data[, c(step1$id.var, step1$time.var)]
    res.fish.loop2 <- cbind(res.fish.loop2, res.fish.loop)
    if (parallel == TRUE) {
      if (cores == 1) {
        registerDoSEQ()
      } else {
        registerDoParallel(cores = cores)
      }
    } else {
      registerDoSEQ()
    }
    indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), 
      .export = c("fdiv")) %dopar% {
      cbind(res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.fish.loop2[res.fish.loop2[, 
        2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.fish.loop2[res.fish.loop2[, 
        2] == year.vec[ano], c("P", "W")]/(res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano - 
        1], c("REV", "COST")]/res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano], 
        c("Qs", "Xs")]), TT = res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano], 
        "TT"]/fdiv(res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano - 1], c("REV", 
        "COST")]/res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), 
        res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano], c("AO", "AI")]/res.fish.loop2[res.fish.loop2[, 
          2] == year.vec[ano], c("Qs", "Xs")], res.fish.loop2[res.fish.loop2[, 2] == 
          year.vec[ano], c("TFP", "MP", "TFPE")]/res.fish.loop2[res.fish.loop2[, 2] == 
          year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.fish.loop2[res.fish.loop2[, 
          2] == year.vec[ano], 14:15]/res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano - 
          1], 14:15], res.fish.loop2[res.fish.loop2[, 2] == year.vec[ano], 16:20]/res.fish.loop2[res.fish.loop2[, 
          2] == year.vec[ano], 28:32])
    }
    
    registerDoSEQ()
    stopImplicitCluster()
    
    indices.1 <- round(indices.1, 4)
    INT <- matrix(nrow = dim(res.fish.loop2[res.fish.loop2[, 2] == year.vec[1], ])[1], 
      ncol = 18)
    INT <- as.data.frame(INT)
    names(INT) <- names(indices.1)
    indices.1 <- rbind(INT, indices.1)
    
    indices.2 <- fish.data[, 1:2]
    indices.2 <- cbind(indices.2, indices.1)
    names(indices.2)[3:20] <- paste0("d", names(fish.data[, 3:20]))
    Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)], res.fish.loop[, 31:(dim(res.fish.loop)[2])])
    names(Shadowp) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
      res.tfp <- list(Levels = fish.data, Changes = indices.2, Shadowp = Shadowp)
    
    class(res.tfp) <- c("list", "Fisher")
    return(res.tfp)
}
