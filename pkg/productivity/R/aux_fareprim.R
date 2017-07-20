### Färe-Primont (FP) first step FP (with technical change)

fp.1 <- function(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled, PRICESO, PRICESI) {
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
    if (scaled == TRUE) {
      Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
      X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
    } else {
      Y.ini <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
      X.ini <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
    }
  }
  
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), 
    .combine = rbind, .packages = c("Rglpk")) %dopar% {
      if (parallel == FALSE) {
        cat("\r")
        cat('Progress:', ano/length(year.vec)*100, '%', '\r')
        flush.console()
        if(ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == 
        year.vec[ano], step1$id.var])) cat('DONE!          \n\r')
      }
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, 
      rts)
    TFPE <- TFP/MP
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, 
        rts)/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      if (length(step1) == 6) {
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
          TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME)
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, 
          OME = OME, ROSE = ROSE, OSME = OSME, RME = RME)
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          PRICESI, rts))/ITE
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
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, 
          AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, 
          ISME = ISME, RME = RME)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, 
          IME = IME, RISE = RISE, ISME = ISME, RME = RME)
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, 
          rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        OME.IME <- sqrt(OME * IME)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- sqrt(OME.IME * ROSE.RISE)
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, 
          AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, 
          OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, 
          OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
          RME = RME)
        }
      }
    }
    return(res1)
  }
  res2
}

### Färe-Primont (FP) first step FP without technical change

fp.2 <- function(data, data.in, step1, rts, orientation, parallel, scaled, PRICESO, PRICESI) {
  X1 <- t(as.matrix(data[, step1$x.vars]))
  Y1 <- t(as.matrix(data[, step1$y.vars]))
  XREF1 <- X1
  YREF1 <- Y1
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[, step1$p.vars]))
    W1 <- t(as.matrix(data[, step1$w.vars]))
    if (scaled == TRUE) {
      Y.ini <- t(as.matrix(data.in[, step1$y.vars]))
      X.ini <- t(as.matrix(data.in[, step1$x.vars]))
    } else {
      Y.ini <- t(as.matrix(data[, step1$y.vars]))
      X.ini <- t(as.matrix(data[, step1$x.vars]))
    }
  }
  res2 <- foreach(dmu = 1:length(data[, step1$id.var]), .combine = rbind, .packages = c("Rglpk"), 
    .export = c("DO.sh", "DO.ome", "DI.sh", "DI.ime", "D.tfp")) %dopar% {
      if (parallel == FALSE) {
        cat("\r")
        cat('Progress:', round(dmu/length(data[, step1$id.var])*100,0), '%', '\r')
        flush.console()
        if(dmu == length(data[, step1$id.var])) cat('DONE!          \n\r')
      }
    AO <- sum(PRICESO * Y1[, dmu])
    AI <- sum(PRICESI * X1[, dmu])
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, 
      rts)
    TFPE <- TFP/MP
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, 
        rts)/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      if (length(step1) == 6) {
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
          TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME)
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, 
          OME = OME, ROSE = ROSE, OSME = OSME, RME = RME)
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          PRICESI, rts))/ITE
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
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, 
          AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, 
          ISME = ISME, RME = RME)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, 
          IME = IME, RISE = RISE, ISME = ISME, RME = RME)
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, 
          rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        OME.IME <- sqrt(OME * IME)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- sqrt(OME.IME * ROSE.RISE)
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, 
          AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, 
          OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, 
          OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
          RME = RME)
        }
      }
    }
    return(res1)
  }
  res2
}

### Färe-Primont (FP), print fonction
print.FarePrimont <- function(x, digits = NULL, ...) {
    if (is.null(digits)) {
        digits <- max(3, getOption("digits") - 3)
    }
    cat("\nF\u00e4re-Primont productivity and profitability levels (summary):\n\n")
    print(summary(x[["Levels"]], digits = digits), digits = digits)
    cat("\n\nF\u00e4re-Primont productivity and profitability changes (summary):\n\n")
    print(summary(x[["Changes"]], digits = digits), digits = digits)
    cat("\n\nF\u00e4re-Primont productivity shadow prices:\n\n")
    print(x[["Shadowp"]], digits = digits)
    cat("\n")
    invisible(x)
}