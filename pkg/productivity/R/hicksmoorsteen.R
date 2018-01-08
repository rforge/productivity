# Hicks-Moorsteen productivity index (hicksmoorsteen)

hicksmoorsteen <- function(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), parallel = FALSE, 
  cores = max(1, detectCores() - 1), scaled = FALSE) {
  step1 <- check.1(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var)) 
    stop("Hicks-Moorsteen index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
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
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 1e-04)) 
      warning("Some quantity variables are not between 1e-4 and 1e5. We recommend rescaling the data or set the scaled option to TRUE to avoid numerical problems\n\r", 
        call. = FALSE)
  } else {
    data.in <- data[, c(step1$time.var, step1$y.vars, step1$x.vars)]
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)], 2, FUN = function(x) x/mean(x))
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
    res.hm.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", "doParallel"), 
      .export = c("hm.1", "DO.shdu", "DI.shdu", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% {
      hm.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled)
    }
  } else {
    res.hm.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", "doParallel"), 
      .export = c("hm.2", "DO.shdu", "DI.shdu", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% {
      hm.2(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled)
    }
  }
  
  registerDoSEQ()
  stopImplicitCluster()
  
  row.names(res.hm.loop) <- seq(1:dim(res.hm.loop)[1])
  hm.data <- data[, c(step1$id.var, step1$time.var)]
  hm.data <- if ("REV" %in% colnames(res.hm.loop)) {
    cbind(hm.data, round.up(res.hm.loop[, 1:17], 4))
  } else {
    cbind(hm.data, round.up(res.hm.loop[, 1:11], 4))
  }
  res.hm.loop2 <- data[, c(step1$id.var, step1$time.var)]
  res.hm.loop2 <- cbind(res.hm.loop2, res.hm.loop)
  
  if (parallel == TRUE) {
    if (cores == 1) {
      registerDoSEQ()
    } else {
      registerDoParallel(cores = cores)
    }
  } else {
    registerDoSEQ()
  }
  
  if ("REV" %in% colnames(res.hm.loop2)) {
    
    indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
      {
        cbind(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
          c("P", "W")]/(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], c("REV", "COST")]/res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], c("Qs", "Xs")]), TT = res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], "TT"]/fdiv(res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano - 1], c("REV", "COST")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("Qs", 
          "Xs")]), res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("AO", "AI")]/res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], c("Qs", "Xs")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("TFP", 
          "MP", "TFPE")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], 14:15]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 14:15], res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], 16:19]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 27:30])
      }
  } else {
    indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
      {
        cbind(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("AO", "AI")]/res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], c("Qs", "Xs")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("TFP", 
          "MP", "TFPE")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], 8:9]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 8:9], res.hm.loop2[res.hm.loop2[, 
          2] == year.vec[ano], 10:13]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 21:24])
      }
  }
  
  registerDoSEQ()
  stopImplicitCluster()
  
  indices.1 <- round.up(indices.1, 4)
  INT <- matrix(nrow = dim(res.hm.loop2[res.hm.loop2[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.1)[2] == 
    11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.1)
  indices.1 <- rbind(INT, indices.1)
  
  indices.2 <- hm.data[, 1:2]
  indices.2 <- cbind(indices.2, indices.1)
  names(indices.2)[3:if (dim(indices.1)[2] == 11) {
    13
  } else {
    19
  }] <- paste0("d", names(hm.data[3:if (dim(indices.1)[2] == 11) {
    13
  } else {
    19
  }]))
  Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)], if (dim(hm.data)[2] == 19) {
    round.up(res.hm.loop[, 29:(dim(res.hm.loop)[2])], 4)
  } else {
    round.up(res.hm.loop[, 23:(dim(res.hm.loop)[2])], 4)
  })
  names(Shadowp) <- c(names(data[id.var]), names(data[time.var]), paste0(names(data[x.vars]), ".ms"), paste0(names(data[y.vars]), 
    ".ms"), paste0(names(data[x.vars]), ".nt"), paste0(names(data[y.vars]), ".nt"))
  res.tfp <- list(Levels = hm.data, Changes = indices.2, Shadowp = Shadowp)
  class(res.tfp) <- c("list", "HicksMoorsteen")
  return(res.tfp)
}
