# Hicks-Moorsteen productivity index (hicksmoorsteen)

hicksmoorsteen <- function(data, id.var, time.var, x.vars, y.vars, w.vars = NULL, p.vars = NULL, tech.change = TRUE, 
  tech.reg = TRUE, rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in", "in-out"), parallel = FALSE, 
  cores = max(1, detectCores() - 1), scaled = FALSE, components = FALSE, shadow= FALSE) {
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
    res.hm.loop.mat <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI", "doParallel"), 
      .export = c("hm.1", "DO.shdu", "DI.shdu", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% {
      hm.1(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled, components)
    }
  } else {
    res.hm.loop.mat <- foreach(ano = 1:(length(year.vec)), .combine = rbind, .packages = c("lpSolveAPI", "doParallel"), 
      .export = c("hm.2", "DO.shdu", "DI.shdu", "DO.sh", "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% {
      hm.2(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled, components)
    }
  }
  
  if (components == FALSE) {
    row.names(res.hm.loop.mat) <- seq(1:dim(res.hm.loop.mat)[1])
    hm.data <- data[, c(step1$id.var, step1$time.var)]
    hm.data <- if ("REV" %in% colnames(res.hm.loop.mat)) {
      cbind(hm.data, round.up(res.hm.loop.mat[, 1:17], 4))
    } else {
      cbind(hm.data, round.up(res.hm.loop.mat[, 1:11], 4))
    }
    res.hm.loop2 <- data[, c(step1$id.var, step1$time.var)]
    res.hm.loop2 <- cbind(res.hm.loop2, res.hm.loop.mat)
    
    if ("REV" %in% colnames(res.hm.loop2)) {
      
      indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), 
                           .export = "fdiv") %dopar% {
                    cbind(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("REV", "COST", "PROF")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 
                    c("REV", "COST", "PROF")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("P", "W")]/(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 
                    c("REV", "COST")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("Qs", "Xs")]), TT = res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    "TT"]/fdiv(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 
                    c("REV", "COST")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("Qs", "Xs")]), res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("AO", "AI")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("Qs", "Xs")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("TFP", "MP", "TFPE")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    c("TFP2", "MP2", "TFPE2")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                    14:15]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 14:15], 
                    res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 16:19]/res.hm.loop2[res.hm.loop2[, 
                    2] == year.vec[ano], 27:30])
                           }
    } else {
      indices.1 <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), 
                           .export = "fdiv") %dopar% {
                             cbind(res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                             c("AO", "AI")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                             c("Qs", "Xs")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                             c("TFP", "MP", "TFPE")]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                             c("TFP2", "MP2", "TFPE2")], res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 
                             8:9]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano - 1], 8:9], 
                             res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 10:13]/res.hm.loop2[res.hm.loop2[, 2] == year.vec[ano], 21:24])
                           }
    }
    registerDoSEQ()
    stopImplicitCluster()
    
    indices.1 <- round.up(indices.1, 4)
    INT <- matrix(nrow = dim(res.hm.loop2[res.hm.loop2[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.1)[2] == 11) {
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
    
    res.tfp <- list(Levels = hm.data, Changes = indices.2)
    class(res.tfp) <- c("list", "HicksMoorsteen")
    
  } else {
    if (dim(res.hm.loop.mat)[2] %% 2 !=0) {
      res.mal_hs.loop <- res.hm.loop.mat[, 1:((dim(res.hm.loop.mat)[2]-3)/2 + 3)]
      res.mal_it.loop <- res.hm.loop.mat[, c(1:3, (((dim(res.hm.loop.mat)[2]-3)/2 + 3) + 1):dim(res.hm.loop.mat)[2])]
    } else {
      res.mal_hs.loop <- res.hm.loop.mat[, 1:(dim(res.hm.loop.mat)[2]/2)]
      res.mal_it.loop <- res.hm.loop.mat[, (dim(res.hm.loop.mat)[2]/2 + 1):dim(res.hm.loop.mat)[2]]
    }
    
  row.names(res.mal_hs.loop) <- seq(1:dim(res.mal_hs.loop)[1])
  mal_hs.data <- data[, c(step1$id.var, step1$time.var)]
  mal_hs.data <- if ("REV" %in% colnames(res.mal_hs.loop)) {
    cbind(mal_hs.data, round.up(res.mal_hs.loop[, 1:17], 4))
  } else {
    cbind(mal_hs.data, round.up(res.mal_hs.loop[, 1:11], 4))
  }
  names(mal_hs.data) <- gsub('.hs', "", names(mal_hs.data))
  res.mal_hs.loop2 <- data[, c(step1$id.var, step1$time.var)]
  res.mal_hs.loop2 <- cbind(res.mal_hs.loop2, res.mal_hs.loop)
  names(res.mal_hs.loop2) <- gsub('.hs', "", names(res.mal_hs.loop2))
  
  row.names(res.mal_it.loop) <- seq(1:dim(res.mal_it.loop)[1])
  mal_it.data <- data[, c(step1$id.var, step1$time.var)]
  mal_it.data <- if ("REV" %in% colnames(res.mal_it.loop)) {
    cbind(mal_it.data, round.up(res.mal_it.loop[, 1:17], 4))
  } else {
    cbind(mal_it.data, round.up(res.mal_it.loop[, 1:11], 4))
  }
  names(mal_it.data) <- gsub('.it', "", names(mal_it.data))
  res.mal_it.loop2 <- data[, c(step1$id.var, step1$time.var)]
  res.mal_it.loop2 <- cbind(res.mal_it.loop2, res.mal_it.loop)
  names(res.mal_it.loop2) <- gsub('.it', "", names(res.mal_it.loop2))
  
  res.hm.loop <- sqrt(res.mal_hs.loop[, -c((dim(res.mal_hs.loop)[2] + 1 - 
  length(x.vars) - length(y.vars)):dim(res.mal_hs.loop)[2])] *  
    res.mal_it.loop[, -c((dim(res.mal_it.loop)[2] + 1 - length(x.vars) - 
    length(y.vars)):dim(res.mal_it.loop)[2])])
  row.names(res.hm.loop) <- seq(1:dim(res.hm.loop)[1])
  hm.data <- data[, c(step1$id.var, step1$time.var)]
  hm.data <- if ("REV" %in% colnames(res.hm.loop)) {
    cbind(hm.data, round.up(res.hm.loop[, 1:17], 4))
  } else {
    cbind(hm.data, round.up(res.hm.loop[, 1:11], 4))
  }
  names(hm.data) <- gsub('.hs', "", names(hm.data))
  res.hm.loop2 <- data[, c(step1$id.var, step1$time.var)]
  res.hm.loop2 <- cbind(res.hm.loop2, res.hm.loop)
  names( res.hm.loop2) <- gsub('.hs', "", names( res.hm.loop2))
  
  if ("REV" %in% colnames(res.mal_hs.loop2)) {
    
    indices.1.hs <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
    {
      cbind(res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.mal_hs.loop2[res.mal_hs.loop2[, 
      2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.mal_hs.loop2[res.mal_hs.loop2[, 2] == 
      year.vec[ano], c("P", "W")]/(res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano - 1], c("REV", 
      "COST")]/res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), TT = res.mal_hs.loop2[res.mal_hs.loop2[, 
      2] == year.vec[ano], "TT"]/fdiv(res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano - 1], 
     c("REV", "COST")]/res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), res.mal_hs.loop2[res.mal_hs.loop2[, 
     2] == year.vec[ano], c("AO", "AI")]/res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], 
    c("Qs", "Xs")], res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], c("TFP", "MP", "TFPE")]/res.mal_hs.loop2[res.mal_hs.loop2[, 
   2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], 
  14:15]/res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano - 1], 14:15], res.mal_hs.loop2[res.mal_hs.loop2[, 
  2] == year.vec[ano], 16:19]/res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], 27:30])
    }
  } else {
    indices.1.hs <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
    {
      cbind(res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], c("AO", "AI")]/res.mal_hs.loop2[res.mal_hs.loop2[, 
      2] == year.vec[ano], c("Qs", "Xs")], res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], 
      c("TFP", "MP", "TFPE")]/res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], c("TFP2", "MP2", 
      "TFPE2")], res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], 8:9]/res.mal_hs.loop2[res.mal_hs.loop2[, 
      2] == year.vec[ano - 1], 8:9], res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[ano], 10:13]/res.mal_hs.loop2[res.mal_hs.loop2[, 
      2] == year.vec[ano], 21:24])
    }
  }
  
  if ("REV" %in% colnames(res.mal_it.loop2)) {
    
    indices.1.it <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
    {
      cbind(res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], c("REV", "COST", "PROF")]/res.mal_it.loop2[res.mal_it.loop2[, 
      2] == year.vec[ano - 1], c("REV", "COST", "PROF")], res.mal_it.loop2[res.mal_it.loop2[, 2] == 
      year.vec[ano], c("P", "W")]/(res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano - 1], c("REV", 
     "COST")]/res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), TT = res.mal_it.loop2[res.mal_it.loop2[, 
     2] == year.vec[ano], "TT"]/fdiv(res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano - 1], 
     c("REV", "COST")]/res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], c("Qs", "Xs")]), res.mal_it.loop2[res.mal_it.loop2[, 
     2] == year.vec[ano], c("AO", "AI")]/res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], 
     c("Qs", "Xs")], res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], c("TFP", "MP", "TFPE")]/res.mal_it.loop2[res.mal_it.loop2[, 
    2] == year.vec[ano], c("TFP2", "MP2", "TFPE2")], res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], 
    14:15]/res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano - 1], 14:15], res.mal_it.loop2[res.mal_it.loop2[, 
    2] == year.vec[ano], 16:19]/res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], 27:30])
    }
  } else {
    indices.1.it <- foreach(ano = 2:(length(year.vec)), .combine = rbind, .packages = c("doParallel"), .export = "fdiv") %dopar% 
    {
      cbind(res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], c("AO", "AI")]/res.mal_it.loop2[res.mal_it.loop2[, 
      2] == year.vec[ano], c("Qs", "Xs")], res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], 
      c("TFP", "MP", "TFPE")]/res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], c("TFP2", "MP2", 
      "TFPE2")], res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], 8:9]/res.mal_it.loop2[res.mal_it.loop2[, 
      2] == year.vec[ano - 1], 8:9], res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[ano], 10:13]/res.mal_it.loop2[res.mal_it.loop2[, 
      2] == year.vec[ano], 21:24])
    }
  }
  
  registerDoSEQ()
  stopImplicitCluster()
  
  #HM
  indices.1.hm <- sqrt(indices.1.hs * indices.1.it)
  indices.1.hm <- round.up(indices.1.hm, 4)
  INT <- matrix(nrow = dim(res.hm.loop2[res.hm.loop2[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.1.hm)[2] == 
      11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.1.hm)
  indices.1.hm <- rbind(INT, indices.1.hm)
  
  indices.2.hm <- hm.data[, 1:2]
  indices.2.hm <- cbind(indices.2.hm, indices.1.hm)
  names(indices.2.hm)[3:if (dim(indices.1.hm)[2] == 11) {
    13
  } else {
    19
  }] <- paste0("d", names(hm.data[3:if (dim(indices.1.hm)[2] == 11) {
    13
  } else {
    19
  }]))
  
  #HS
  indices.1.hs <- round.up(indices.1.hs, 4)
  INT <- matrix(nrow = dim(res.mal_hs.loop2[res.mal_hs.loop2[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.1.hs)[2] == 
    11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.1.hs)
  indices.1.hs <- rbind(INT, indices.1.hs)
  
  indices.2.hs <- mal_hs.data[, 1:2]
  indices.2.hs <- cbind(indices.2.hs, indices.1.hs)
  names(indices.2.hs)[3:if (dim(indices.1.hs)[2] == 11) {
    13
  } else {
    19
  }] <- paste0("d", names(mal_hs.data[3:if (dim(indices.1.hs)[2] == 11) {
    13
  } else {
    19
  }]))
  
  #IT
  indices.1.it <- round.up(indices.1.it, 4)
  INT <- matrix(nrow = dim(res.mal_it.loop2[res.mal_it.loop2[, 2] == year.vec[1], ])[1], ncol = if (dim(indices.1.it)[2] == 
    11) {
    11
  } else {
    17
  })
  INT <- as.data.frame(INT)
  names(INT) <- names(indices.1.it)
  indices.1.it <- rbind(INT, indices.1.it)
  
  indices.2.it <- mal_it.data[, 1:2]
  indices.2.it <- cbind(indices.2.it, indices.1.it)
  names(indices.2.it)[3:if (dim(indices.1.it)[2] == 11) {
    13
  } else {
    19
  }] <- paste0("d", names(mal_it.data[3:if (dim(indices.1.it)[2] == 11) {
    13
  } else {
    19
  }]))
  
  if (shadow == TRUE) {
    Shadowp.hs <- cbind(data[, c(step1$id.var, step1$time.var)], if (dim(mal_hs.data)[2] == 19) {
      round.up(res.mal_hs.loop[, 29:(dim(res.mal_hs.loop)[2])], 4)
    } else {
      round.up(res.mal_hs.loop[, 23:(dim(res.mal_hs.loop)[2])], 4)
    })
    names(Shadowp.hs) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
  
    Shadowp.it <- cbind(data[, c(step1$id.var, step1$time.var)], if (dim(mal_it.data)[2] == 19) {
      round.up(res.mal_it.loop[, 29:(dim(res.mal_it.loop)[2])], 4)
    } else {
      round.up(res.mal_it.loop[, 23:(dim(res.mal_it.loop)[2])], 4)
    })
    names(Shadowp.it) <- c(names(data[id.var]), names(data[time.var]), names(data[x.vars]), names(data[y.vars]))
  
    res.tfp.hs <- list(Levels = mal_hs.data, Changes = indices.2.hs, Shadowp = Shadowp.hs)
    res.tfp.it <- list(Levels = mal_it.data, Changes = indices.2.it, Shadowp = Shadowp.it)
  } else {
    res.tfp.hs <- list(Levels = mal_hs.data, Changes = indices.2.hs)
    res.tfp.it <- list(Levels = mal_it.data, Changes = indices.2.it)
  }
  res.tfp.hm <- list(Levels = hm.data, Changes = indices.2.hm)
  res.tfp <- list(HicksMoorsteen = res.tfp.hm, MalmquistHS = res.tfp.hs, MalmquistIT = res.tfp.it)
  class(res.tfp) <- c("list", "HicksMoorsteen")
  }
  return(res.tfp)
  }
