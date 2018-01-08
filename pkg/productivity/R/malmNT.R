# Malmquist-nt productivity index (malmNT)

malmNT <- function(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, tech.change = TRUE, tech.reg = TRUE, 
  rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), parallel = FALSE, cores = max(1, 
    detectCores() - 1), scaled = FALSE) {
  step1 <- check.1(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars)
  if (!balanced(data = data, id.var = id.var, time.var = time.var)) 
    stop("Malmquist-NT index can only be computed from balanced data. Please consider balancing the data.", call. = FALSE)
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
      warning("Some quantity variables are not between 1e-4 and 1e5. We recommend rescaling the data or set the scaled option to TRUE to avoid numerical problent\n\r", 
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
    res.mal_nt.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", "doParallel"), 
      .export = c("malm.nt.1", "DO.shdu", "DI.shdu", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% 
      {
        malm.nt.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled)
      }
  } else {
    res.mal_nt.loop <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("Rglpk", "doParallel"), 
      .export = c("malm.nt.2", "DO.shdu", "DI.shdu", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% 
      {
        malm.nt.2(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled)
      }
  }
  
  registerDoSEQ()
  stopImplicitCluster()
  
  row.names(res.mal_nt.loop) <- seq(1:dim(res.mal_nt.loop)[1])
  mal_nt.data <- data[, c(step1$id.var, step1$time.var)]
  mal_nt.data <- if ("REV" %in% colnames(res.mal_nt.loop)) {
    cbind(mal_nt.data, round.up(res.mal_nt.loop[, 1:17], 4))
  } else {
    cbind(mal_nt.data, round.up(res.mal_nt.loop[, 1:11], 4))
  }
  res.mal_nt.loop2 <- data[, c(step1$id.var, step1$time.var)]
  res.mal_nt.loop2 <- cbind(res.mal_nt.loop2, res.mal_nt.loop)
  
  if (parallel == TRUE) {
    if (cores == 1) {
      registerDoSEQ()
    } else {
      registerDoParallel(cores = cores)
    }
  } else {
    registerDoSEQ()
  }
  
  if ("REV" %in% colnames(res.mal_nt.loop2)) {
    
    indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
      {
        cbind(res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.mal_nt.loop2[res.mal_nt.loop2[, 2] == 
          year.vec[ano], c("P", "W")]/(res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano - 1], c("REV", 
          "COST")]/res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), TT = res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano], "TT"]/fdiv(res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano - 1], 
          c("REV", "COST")]/res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano], c("AO", "AI")]/res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], 
          c("Qs", "Xs")], res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], c("TFP", "MP", "TFPE")]/res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], 
          14:15]/res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano - 1], 14:15], res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano], 16:19]/res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], 27:30])
      }
  } else {
    indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
      {
        cbind(res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], c("AO", "AI")]/res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano], c("Qs", "Xs")], res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], 
          c("TFP", "MP", "TFPE")]/res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], c("TFP2", "MP2", 
          "TFPE2")], res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], 8:9]/res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano - 1], 8:9], res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[ano], 10:13]/res.mal_nt.loop2[res.mal_nt.loop2[, 
          2] == year.vec[ano], 21:24])
      }
  }
  
  registerDoSEQ()
  stopImplicitCluster()
  
  indices.1 <- round.up(indices.1, 4)
  INT <- matrix(nrow = dim(res.mal_nt.loop2[res.mal_nt.loop2[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.1)[2] == 
    11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.1)
  indices.1 <- rbind(INT, indices.1)
  
  indices.2 <- mal_nt.data[, 1:2]
  indices.2 <- cbind(indices.2, indices.1)
  names(indices.2)[3:if (dim(indices.1)[2] == 11) {
    13
  } else {
    19
  }] <- paste0("d", names(mal_nt.data[3:if (dim(indices.1)[2] == 11) {
    13
  } else {
    19
  }]))
  Shadowp <- cbind(data[, c(step1$id.var, step1$time.var)], if (dim(mal_nt.data)[2] == 19) {
    round.up(res.mal_nt.loop[, 29:(dim(res.mal_nt.loop)[2])], 4)
  } else {
    round.up(res.mal_nt.loop[, 23:(dim(res.mal_nt.loop)[2])], 4)
  })
  names(Shadowp) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
  res.tfp <- list(Levels = mal_nt.data, Changes = indices.2, Shadowp = Shadowp)
  class(res.tfp) <- c("list", "MalmquistNT")
  return(res.tfp)
}
