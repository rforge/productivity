## Classic Malmquist productivity index

malm <- function(data, id.var, time.var, x.vars, y.vars, tech.reg = TRUE, 
rts = c("vrs", "crs", "nirs", "ndrs"), orientation = c("out", "in"), cores = detectCores() - 1, 
scaled = TRUE, indices = FALSE, out.levels = NULL, out.indices = NULL) {
  step1 <- check.3(data, id.var, time.var, x.vars, y.vars)
  rts <- match.arg(rts)
  RTS <- c("vrs", "crs", "nirs", "ndrs")
  if (!(rts %in% RTS)) 
    stop("Unknown scale of returns: ", paste(rts), call. = FALSE)
  year.vec <- unique(data[, time.var])
  orientation <- match.arg(orientation)
  ORIENTATION <- c("out", "in")
  if (!(orientation %in% ORIENTATION)) 
    stop("Unknown orientation: ", paste(orientation), call. = FALSE)
  if (scaled == FALSE) {
    if (any(data[, c(step1$x.vars, step1$y.vars)] >= 1e+05 | data[, c(step1$x.vars, step1$y.vars)] <= 1e-04)) 
      warning("Some quantity variables are not between 1e-4 and 1e5. We recommend rescaling the data 
or set the scaled option to TRUE to avoid numerical problems", call. = FALSE)
  } else {
    data[, c(step1$x.vars, step1$y.vars)] <- apply(data[, c(step1$x.vars, step1$y.vars)], 
      2, FUN = function(x) x/mean(x))
  }
    if (cores <= 1) {
      cores <- 1
    }
  registerDoParallel(cores = cores)
  res_malm_loop <- foreach(ano = 1:(length(year.vec) - 1), .combine = rbind, .packages = c("Rglpk", 
    "doParallel"), .export = c("malm.1", "DO.sh", "DI.sh")) %dopar% {
    malm.1(data, step1, ano, year.vec, tech.reg, rts, orientation)
  }
  registerDoSEQ()
  stopImplicitCluster()
  malm.data <- data[data[, step1$time.var] %in% year.vec[-1], c(step1$id.var, step1$time.var)]
  names(malm.data)[2] <- "Year.1"
  malm.data <- cbind(malm.data, res_malm_loop)
  row.names(malm.data) <- seq(1:dim(malm.data)[1])
  if (!is.null(out.levels)) 
    write.csv(malm.data, file = out.levels)
  if (indices == FALSE) {
    res.tfp <- list(Levels = malm.data)
    class(res.tfp) <- c("list", "Malmquist")
    return(res.tfp)
  } else {
    data.indices <- malm.data[, 1:3]
    if (orientation == "out") {
    data.indices[, "malmquist"] <- ((malm.data[, "c111o"]/malm.data[, "c100o"]) * (malm.data[, 
      "c011o"]/malm.data[, "c000o"]))^0.5
    data.indices[, "effch"] <- malm.data[, "c111o"]/malm.data[, "c000o"]
    data.indices[, "tech"] <- ((malm.data[, "c011o"]/malm.data[, "c111o"]) * (malm.data[, 
      "c000o"]/malm.data[, "c100o"]))^0.5
    data.indices[, "obtech"] <- ((malm.data[, "c011o"]/malm.data[, "c111o"]) * (malm.data[, 
      "c110o"]/malm.data[, "c010o"]))^0.5
    data.indices[, "ibtech"] <- ((malm.data[, "c100o"]/malm.data[, "c000o"]) * (malm.data[, 
      "c010o"]/malm.data[, "c110o"]))^0.5
    data.indices[, "matech"] <- malm.data[, "c000o"]/malm.data[, "c100o"]
    if (dim(malm.data)[2] > 9) {
        data.indices[, "pure.out.effch"] <- malm.data[, "v111o"]/malm.data[, "v000o"]
        data.indices[, "out.scalech"] <- (malm.data[, "c111o"]/malm.data[, "c000o"])/(malm.data[, "v111o"]/malm.data[, "v000o"])
      }
    } else {
    data.indices[, "malmquist"] <- ((malm.data[, "c100i"]/malm.data[, "c111i"]) * (malm.data[, 
        "c000i"]/malm.data[, "c011i"]))^0.5
      data.indices[, "effch"] <- malm.data[, "c000i"]/malm.data[, "c111i"]
      data.indices[, "tech"] <- ((malm.data[, "c111i"]/malm.data[, "c011i"]) * (malm.data[, 
        "c100i"]/malm.data[, "c000i"]))^0.5
      data.indices[, "obtech"] <- ((malm.data[, "c111i"]/malm.data[, "c011i"]) * (malm.data[, 
        "c010i"]/malm.data[, "c110i"]))^0.5
      data.indices[, "ibtech"] <- ((malm.data[, "c000i"]/malm.data[, "c100i"]) * (malm.data[, 
        "c110i"]/malm.data[, "c010i"]))^0.5
      data.indices[, "matech"] <- malm.data[, "c100i"]/malm.data[, "c000i"]
      if (dim(malm.data)[2] > 9) {
        data.indices[, "pure.inp.effch"] <- malm.data[, "v000i"]/malm.data[, "v111i"]
        data.indices[, "inp.scalech"] <- (malm.data[, "c000i"]/malm.data[, "c111i"])/(malm.data[, "v000i"]/malm.data[, "v111i"])
      }
    }
    if (!is.null(out.indices)) 
      write.csv(data.indices, file = out.indices)
    res.tfp <- list(Levels = malm.data, Indices = data.indices)
    class(res.tfp) <- c("list", "Malmquist")
    return(res.tfp)
  }
}
