### Färe-Primont (FP), Lowe and malmquist auxiliary functions

## Färe-Primont (FP) auxiliary functions dual FP output
du.fpo <- function(Xs, Ys, XREFs, YREFs, rts) {
  n_x <- dim(XREFs)[1]
  n_y <- dim(YREFs)[1]
  n_t <- dim(XREFs)[2]
  if (rts == "vrs") {
    objo <- c(rep(0, n_y), Xs, 1)
    mato <- rbind(cbind(-t(YREFs), t(XREFs), matrix(1, nrow = n_t, ncol = 1)), c(Ys, rep(0, n_x), 0))
    diro <- c(rep(">=", n_t), "==")
    rhso <- c(rep(0, n_t), 1)
    maxo <- FALSE
    boundso <- list(lower = list(ind = (n_y + n_x + 1), val = -Inf), upper = list(ind = (n_y + n_x + 1), val = Inf))
    opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo, bounds = boundso)
    prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * Xs) + opto$solution[n_y + n_x + 1])
  } else {
    if (rts == "nirs") {
      objo <- c(rep(0, n_y), Xs, 1)
      mato <- rbind(cbind(-t(YREFs), t(XREFs), matrix(1, nrow = n_t, ncol = 1)), c(Ys, rep(0, n_x), 0))
      diro <- c(rep(">=", n_t), "==")
      rhso <- c(rep(0, n_t), 1)
      maxo <- FALSE
      opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo)
      prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * Xs) + opto$solution[n_y + n_x + 
        1])
    } else {
      if (rts == "ndrs") {
        objo <- c(rep(0, n_y), Xs, -1)
        mato <- rbind(cbind(-t(YREFs), t(XREFs), matrix(-1, nrow = n_t, ncol = 1)), c(Ys, rep(0, n_x), 0))
        diro <- c(rep(">=", n_t), "==")
        rhso <- c(rep(0, n_t), 1)
        maxo <- FALSE
        opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo)
        prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * Xs) - opto$solution[n_y + n_x + 
          1])
      } else {
        objo <- c(rep(0, n_y), Xs)
        mato <- rbind(cbind(-t(YREFs), t(XREFs)), c(Ys, rep(0, n_x)))
        diro <- c(rep(">=", n_t), "==")
        rhso <- c(rep(0, n_t), 1)
        maxo <- FALSE
        opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo)
        prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * Xs))
      }
    }
  }
  names(prices_o) <- paste("U", 1:n_y, sep = "")
  prices_o
}

# dual FP input
du.fpi <- function(Xs, Ys, XREFs, YREFs, rts) {
  n_x <- dim(XREFs)[1]
  n_y <- dim(YREFs)[1]
  n_t <- dim(XREFs)[2]
  if (rts == "vrs") {
    obji <- c(Ys, rep(0, n_x), 1)
    mati <- rbind(cbind(t(YREFs), -t(XREFs), matrix(1, nrow = n_t, ncol = 1)), c(rep(0, n_y), Xs, 0))
    diri <- c(rep("<=", n_t), "==")
    rhsi <- c(rep(0, n_t), 1)
    maxi <- TRUE
    boundsi <- list(lower = list(ind = (n_y + n_x + 1), val = -Inf), upper = list(ind = (n_y + n_x + 1), val = Inf))
    opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi, bounds = boundsi)
    prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * Ys) + opti$solution[n_x + n_y + 1])
  } else {
    if (rts == "nirs") {
      obji <- c(Ys, rep(0, n_x), -1)
      mati <- rbind(cbind(t(YREFs), -t(XREFs), matrix(-1, nrow = n_t, ncol = 1)), c(rep(0, n_y), Xs, 0))
      diri <- c(rep("<=", n_t), "==")
      rhsi <- c(rep(0, n_t), 1)
      maxi <- TRUE
      opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi)
      prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * Ys) - opti$solution[n_x + n_y + 
        1])
    } else {
      if (rts == "ndrs") {
        obji <- c(Ys, rep(0, n_x), 1)
        mati <- rbind(cbind(t(YREFs), -t(XREFs), matrix(1, nrow = n_t, ncol = 1)), c(rep(0, n_y), Xs, 0))
        diri <- c(rep("<=", n_t), "==")
        rhsi <- c(rep(0, n_t), 1)
        maxi <- TRUE
        opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi)
        prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * Ys) + opti$solution[n_x + n_y + 
          1])
      } else {
        obji <- c(Ys, rep(0, n_x))
        mati <- rbind(cbind(t(YREFs), -t(XREFs)), c(rep(0, n_y), Xs))
        diri <- c(rep("<=", n_t), "==")
        rhsi <- c(rep(0, n_t), 1)
        maxi <- TRUE
        opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi)
        prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * Ys))
      }
    }
  }
  names(prices_i) <- paste("V", 1:n_x, sep = "")
  prices_i
}

# first step FP (with technical change)
fp.1 <- function(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, PRICESO, PRICESI) {
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  if (tech.reg == TRUE) {
    XREF1 <- X1
    YREF1 <- Y1
  } else {
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:ano], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:ano], step1$y.vars]))
  }
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
    W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
    Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
  }
  
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, .packages = c("Rglpk")) %dopar% 
    {
      AO <- sum(PRICESO * Y1[, dmu])
      AI <- sum(PRICESI * X1[, dmu])
      TFP <- AO/AI
      MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
      TFPE <- TFP/MP
      if (orientation == "out") {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME)  #PRICESO, PRICESI
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME)  #PRICESO, PRICESI
        }
      } else {
        if (orientation == "in") {
          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
          IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
          RISE <- (AO/(AI * IME * ITE))/MP
          ISME <- IME * RISE
          RME <- TFPE/ITE/ISE
          if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
            TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME)  #PRICESO, PRICESI
          } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, 
            ISME = ISME, RME = RME)  #PRICESO, PRICESI
          }
        } else {
          OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
          OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
          ROSE <- ((AO/(OTE * OME))/AI)/MP
          OSME <- OME * ROSE
          RME <- TFPE/OTE/OSE
          # OSME <- OSE * RME
          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
          IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
          RISE <- (AO/(AI * IME * ITE))/MP
          ISME <- IME * RISE
          # RME <- TFPE/ITE/ISE
          OTE.ITE <- sqrt(OTE * ITE)
          OSE.ISE <- sqrt(OSE * ISE)
          OME.IME <- sqrt(OME * IME)
          ROSE.RISE <- sqrt(ROSE * RISE)
          OSME.ISME <- OME.IME * ROSE.RISE
          # RME <- TFPE/OTE.ITE/OSE.ISE
          if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
            TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
            RME = RME)  #PRICESO, PRICESI
          } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, 
            ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME)  #PRICESO, PRICESI
          }
        }
      }
      return(res1)
    }
  res2
}

# first step FP without technical change
fp.2 <- function(data, data.in, step1, rts, orientation, PRICESO, PRICESI) {
  X1 <- t(as.matrix(data[, step1$x.vars]))
  Y1 <- t(as.matrix(data[, step1$y.vars]))
  XREF1 <- X1
  YREF1 <- Y1
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[, step1$p.vars]))
    W1 <- t(as.matrix(data[, step1$w.vars]))
    Y.ini <- t(as.matrix(data.in[, step1$y.vars]))
    X.ini <- t(as.matrix(data.in[, step1$x.vars]))
  }
  res2 <- foreach(dmu = 1:length(data[, step1$id.var]), .combine = rbind, .packages = c("Rglpk"), .export = c("DO.sh", 
    "DO.ome", "DI.sh", "DI.ime", "D.tfp")) %dopar% {
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      # OSME <- OSE * RME
      if (length(step1) == 6) {
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME)  #PRICESO, PRICESI
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME)  #PRICESO, PRICESI
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        RME <- TFPE/ITE/ISE
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME)  #PRICESO, PRICESI
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, 
          ISME = ISME, RME = RME)  #PRICESO, PRICESI
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        OME.IME <- sqrt(OME * IME)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- OME.IME * ROSE.RISE
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
          RME = RME)  #PRICESO, PRICESI
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, 
          ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME)  #PRICESO, PRICESI
        }
      }
    }
    return(res1)
  }
  res2
}

## Lowe auxiliary functions first step lowe (with technical change)
lo.1 <- function(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, PRICESO, PRICESI) {
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  if (tech.reg == TRUE) {
    XREF1 <- X1
    YREF1 <- Y1
  } else {
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:ano], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:ano], step1$y.vars]))
  }
  P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
  X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, .packages = c("Rglpk")) %dopar% 
    {
      AO <- sum(PRICESO * Y1[, dmu])
      AI <- sum(PRICESI * X1[, dmu])
      TFP <- AO/AI
      MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
      TFPE <- TFP/MP
      if (orientation == "out") {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME)  #PRICESO, PRICESI
      } else {
        if (orientation == "in") {
          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
          IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
          RISE <- (AO/(AI * IME * ITE))/MP
          ISME <- IME * RISE
          RME <- TFPE/ITE/ISE
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME)  #PRICESO, PRICESI
        } else {
          OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
          OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
          ROSE <- ((AO/(OTE * OME))/AI)/MP
          OSME <- OME * ROSE
          RME <- TFPE/OTE/OSE
          # OSME <- OSE * RME
          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
          IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
          RISE <- (AO/(AI * IME * ITE))/MP
          ISME <- IME * RISE
          OTE.ITE <- sqrt(OTE * ITE)
          OSE.ISE <- sqrt(OSE * ISE)
          OME.IME <- sqrt(OME * IME)
          ROSE.RISE <- sqrt(ROSE * RISE)
          OSME.ISME <- OME.IME * ROSE.RISE
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
          RME = RME)  #PRICESO, PRICESI
        }
      }
      return(res1)
    }
  res2
}

# first step lowe (without technical change)
lo.2 <- function(data, data.in, step1, rts, orientation, PRICESO, PRICESI) {
  X1 <- t(as.matrix(data[, step1$x.vars]))
  Y1 <- t(as.matrix(data[, step1$y.vars]))
  XREF1 <- X1
  YREF1 <- Y1
  P1 <- t(as.matrix(data[, step1$p.vars]))
  W1 <- t(as.matrix(data[, step1$w.vars]))
  Y.ini <- t(as.matrix(data.in[, step1$y.vars]))
  X.ini <- t(as.matrix(data.in[, step1$x.vars]))
  res2 <- foreach(dmu = 1:length(data[, step1$id.var]), .combine = rbind, .packages = c("Rglpk"), .export = c("DO.sh", 
    "DI.sh", "DO.ome", "DI.ime", "D.tfp")) %dopar% {
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      REV <- sum(Y.ini[, dmu] * P1[, dmu])
      COST <- sum(X.ini[, dmu] * W1[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, 
        OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME)  #PRICESO, PRICESI
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        RME <- TFPE/ITE/ISE
        # OSME <- OSE * RME
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME)  #PRICESO, PRICESI
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
          TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
          RME = RME)  #PRICESO, PRICESI
      }
    }
    return(res1)
  }
  res2
}

## Classic malmquist auxiliary functions
malm.1 <- function(data, step1, ano, year.vec, tech.reg, rts, orientation) {
  ## period (Xt1, Yt1)
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano + 1], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano + 1], step1$y.vars]))
  ## period (Xt, Yt)
  X2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  if (tech.reg == TRUE) {
    ## period (Xt1, Yt1)
    XREF1 <- X1
    YREF1 <- Y1
    ## period (Xt, Yt)
    XREF2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
    YREF2 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  } else {
    ## period (Xt1, Yt1)
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano + 1)], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano + 1)], step1$y.vars]))
    ## period (Xt, Yt)
    XREF2 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$x.vars]))
    YREF2 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$y.vars]))
  }
  # Malmquist components
  if (rts == "crs") {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, .packages = c("Rglpk")) %dopar% 
      {
        # n1n2n3: period reference, period input, period output
        if (orientation == "out") {
          c111o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c100o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c011o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          c000o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          c110o <- DO.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c010o <- DO.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          res1 <- c(Year.0 = year.vec[ano], c111o = c111o, c100o = c100o, c011o = c011o, c000o = c000o, c110o = c110o, c010o = c010o)
        } else {
          c111i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c100i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c011i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          c000i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          c110i <- DI.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c010i <- DI.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          res1 <- c(Year.0 = year.vec[ano], c111i = c111i, c100i = c100i, c011i = c011i, c000i = c000i, c110i = c110i, c010i = c010i)
        }
        return(res1)
        
        ## Under CRS ci=1/co
      }
  } else {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, .packages = c("Rglpk")) %dopar% 
      {
        # n1n2n3: period reference, period input, period output
        if (orientation == "out") {
        c111o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
        c100o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
        c011o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
        c000o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
        c110o <- DO.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
        c010o <- DO.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
        v111o <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        v000o <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        res1 <- c(Year.0 = year.vec[ano], c111o = c111o, c100o = c100o, c011o = c011o, c000o = c000o, c110o = c110o, c010o = c010o, v111o = v111o, v000o = v000o)
        } else {
          # n1n2n3: period reference, period input, period output
          c111i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c100i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c011i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          c000i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          c110i <- DI.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")
          c010i <- DI.sh(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")
          v111i <- DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
          v000i <- DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
          res1 <- c(Year.0 = year.vec[ano], c111i = c111i, c100i = c100i, c011i = c011i, c000i = c000i, c110i = c110i, c010i = c010i, v111i = v111i, v000i = v000i)
        }
        return(res1)
      }
  }
  res2
}
