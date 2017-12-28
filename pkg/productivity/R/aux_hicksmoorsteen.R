## Hicks-Moorsteen with technical change

hm.1 <- function(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled) {
  ## period (Xt1, Yt1)
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  ## period (Xt, Yt)
  X2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$x.vars]))
  }
  Y2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$y.vars]))
  }
  if (tech.reg == TRUE) {
    ## period (Xt1, Yt1)
    XREF1 <- X1
    YREF1 <- Y1
    ## period (Xt, Yt)
    XREF2 <- X2
    YREF2 <- Y2
  } else {
    ## period (Xt1, Yt1)
    XREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$x.vars]))
    YREF1 <- t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$y.vars]))
    ## period (Xt, Yt)
    XREF2 <- if (ano == 1) {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$x.vars]))
    } else {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano - 1)], step1$x.vars]))
    }
    YREF2 <- if (ano == 1) {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano)], step1$y.vars]))
    } else {
      t(as.matrix(data[data[, step1$time.var] %in% year.vec[1:(ano - 1)], step1$y.vars]))
    }
  }
  
  ## prices matrix
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
    W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
    Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
  }
  
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
    .packages = c("Rglpk")) %dopar% {
    if (parallel == FALSE) {
      cat("\r")
      cat("Progress:", round.up(ano/length(year.vec) * 100, 0), "%", "\r")
      flush.console()
      if (ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == year.vec[ano], step1$id.var])) 
        cat("DONE!\n\r")
    }
    
    PRICESO.ms <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts)
    PRICESI.ms <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
    PRICESO.nt <- DO.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts)
    PRICESI.nt <- DI.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
    
    names(PRICESO.ms) <- paste0(names(PRICESO.ms), ".ms")
    names(PRICESI.ms) <- paste0(names(PRICESI.ms), ".ms")
    names(PRICESO.nt) <- paste0(names(PRICESO.nt), ".nt")
    names(PRICESI.nt) <- paste0(names(PRICESI.nt), ".nt")
    
    Qt.ms <- sum(PRICESO.ms * Y1[, dmu])
    Qs.ms <- sum(PRICESO.ms * Y2[, dmu])
    Xt.ms <- sum(PRICESI.ms * X1[, dmu])
    Xs.ms <- sum(PRICESI.ms * X2[, dmu])
    Qt.nt <- sum(PRICESO.nt * Y1[, dmu])
    Qs.nt <- sum(PRICESO.nt * Y2[, dmu])
    Xt.nt <- sum(PRICESI.nt * X1[, dmu])
    Xs.nt <- sum(PRICESI.nt * X2[, dmu])
    
    Qt <- sqrt(Qt.ms * Qt.nt)
    Xt <- sqrt(Xt.ms * Xt.nt)
    Qs <- sqrt(Qs.ms * Qs.nt)
    Xs <- sqrt(Xs.ms * Xs.nt)
    
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- sqrt(D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.ms, 
      PRICESI = PRICESI.ms, rts) * D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
      PRICESO = PRICESO.nt, PRICESI = PRICESI.nt, rts))
    TFPE <- TFP/MP
    
    TFP2 <- Qs/Xs
    MP2 <- sqrt(D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.ms, 
      PRICESI = PRICESI.ms, rts) * D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
      PRICESO = PRICESO.nt, PRICESI = PRICESI.nt, rts))
    TFPE2 <- TFP2/MP2
    
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.ms, 
        rts) * DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.nt, 
        rts))/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      # OSME <- OSE * RME
      
      OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
      OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
      OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.ms, 
        rts) * DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.nt, 
        rts))/OTE2
      ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
      OSME2 <- OME2 * ROSE2
      RME2 <- TFPE2/OTE2/OSE2
      
      if (length(step1) == 6) {
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME, Qt = Qt, 
          Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = OME2, ROSE2 = ROSE2, 
          OSME2 = OSME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, 
          OME2 = OME2, ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.nt, 
          rts)))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2
        IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.nt, 
          rts)))/ITE2
        RISE2 <- (Qs/(Xs * IME2 * ITE2))/MP2
        ISME2 <- IME2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME, 
          Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = IME2, RISE2 = RISE2, 
          ISME2 = ISME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, 
          RISE = RISE, ISME = ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
          TFPE2 = TFPE2, IME2 = IME2, RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, 
          PRICESI.nt, PRICESO.nt)
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.ms, 
          rts) * DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.nt, 
          rts))/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        
        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
        OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.ms, 
          rts) * DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.nt, 
          rts))/OTE2
        ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
        OSME2 <- OME2 * ROSE2
        RME2 <- TFPE2/OTE2/OSE2
        
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.nt, 
          rts)))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        # RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2
        IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.nt, 
          rts)))/ITE2
        RISE2 <- (Qs/(Xs * IME2 * ITE2))/MP2
        ISME2 <- IME2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        OME.IME <- sqrt(OME * IME)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- sqrt(OME.IME * ROSE.RISE)
        # RME <- TFPE/OTE.ITE/OSE.ISE
        
        OTE2.ITE2 <- sqrt(OTE2 * ITE2)
        OSE2.ISE2 <- sqrt(OSE2 * ISE2)
        OME2.IME2 <- sqrt(OME2 * IME2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- sqrt(OME2.IME2 * ROSE2.RISE2)
        
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, 
          OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
          TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2, 
          RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, 
          OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, 
          Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, 
          OSME2.ISME2 = OSME2.ISME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
        }
      }
    }
    return(res1)
  }
  res2
}

## Hicks-Moorsteen without technical change

hm.2 <- function(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled) {
  ## period (Xt1, Yt1)
  X1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  Y1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  ## period (Xt, Yt)
  X2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$x.vars]))
  }
  Y2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$y.vars]))
  }
  
  ## prices matrix
  if (length(step1) == 6) {
    P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
    W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
    Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
  }
  
  ## all periods reference
  XREFs <- t(as.matrix(data[, step1$x.vars]))
  YREFs <- t(as.matrix(data[, step1$y.vars]))
  
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
    .packages = c("Rglpk")) %dopar% {
    
    if (parallel == FALSE) {
      cat("\r")
      cat("Progress:", round.up(ano/length(year.vec) * 100, 0), "%", "\r")
      flush.console()
      if (ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == year.vec[ano], step1$id.var])) 
        cat("DONE!\n\r")
    }
    
    PRICESO.ms <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
    PRICESI.ms <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
    PRICESO.nt <- DO.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
    PRICESI.nt <- DI.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
    
    names(PRICESO.ms) <- paste0(names(PRICESO.ms), ".ms")
    names(PRICESI.ms) <- paste0(names(PRICESI.ms), ".ms")
    names(PRICESO.nt) <- paste0(names(PRICESO.nt), ".nt")
    names(PRICESI.nt) <- paste0(names(PRICESI.nt), ".nt")
    
    Qt.ms <- sum(PRICESO.ms * Y1[, dmu])
    Qs.ms <- sum(PRICESO.ms * Y2[, dmu])
    Xt.ms <- sum(PRICESI.ms * X1[, dmu])
    Xs.ms <- sum(PRICESI.ms * X2[, dmu])
    Qt.nt <- sum(PRICESO.nt * Y1[, dmu])
    Qs.nt <- sum(PRICESO.nt * Y2[, dmu])
    Xt.nt <- sum(PRICESI.nt * X1[, dmu])
    Xs.nt <- sum(PRICESI.nt * X2[, dmu])
    
    Qt <- sqrt(Qt.ms * Qt.nt)
    Xt <- sqrt(Xt.ms * Xt.nt)
    Qs <- sqrt(Qs.ms * Qs.nt)
    Xs <- sqrt(Xs.ms * Xs.nt)
    
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- sqrt(D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.ms, 
      PRICESI = PRICESI.ms, rts) * D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, 
      PRICESO = PRICESO.nt, PRICESI = PRICESI.nt, rts))
    TFPE <- TFP/MP
    
    TFP2 <- Qs/Xs
    MP2 <- sqrt(D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.ms, 
      PRICESI = PRICESI.ms, rts) * D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
      PRICESO = PRICESO.nt, PRICESI = PRICESI.nt, rts))
    TFPE2 <- TFP2/MP2
    
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
      OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.ms, 
        rts) * DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.nt, 
        rts))/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      # OSME <- OSE * RME
      
      OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
      OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
      OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.ms, 
        rts) * DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.nt, 
        rts))/OTE2
      ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
      OSME2 <- OME2 * ROSE2
      RME2 <- TFPE2/OTE2/OSE2
      
      if (length(step1) == 6) {
        REV <- sum(Y.ini[, dmu] * P1[, dmu])
        COST <- sum(X.ini[, dmu] * W1[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME, Qt = Qt, 
          Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = OME2, ROSE2 = ROSE2, 
          OSME2 = OSME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, 
          OME2 = OME2, ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE
        IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.nt, 
          rts)))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2
        IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.nt, 
          rts)))/ITE2
        RISE2 <- (Qs/(Xs * IME2 * ITE2))/MP2
        ISME2 <- IME2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME, 
          Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = IME2, RISE2 = RISE2, 
          ISME2 = ISME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, 
          RISE = RISE, ISME = ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
          TFPE2 = TFPE2, IME2 = IME2, RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, 
          PRICESI.nt, PRICESO.nt)
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
        OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.ms, 
          rts) * DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.nt, 
          rts))/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        
        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
        OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.ms, 
          rts) * DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.nt, 
          rts))/OTE2
        ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
        OSME2 <- OME2 * ROSE2
        RME2 <- TFPE2/OTE2/OSE2
        
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE
        IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.nt, 
          rts)))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        # RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2
        IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.ms, 
          rts)) * (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.nt, 
          rts)))/ITE2
        RISE2 <- (Qs/(Xs * IME2 * ITE2))/MP2
        ISME2 <- IME2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        OME.IME <- sqrt(OME * IME)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- OME.IME * ROSE.RISE
        # RME <- TFPE/OTE.ITE/OSE.ISE
        
        OTE2.ITE2 <- sqrt(OTE2 * ITE2)
        OSE2.ISE2 <- sqrt(OSE2 * ISE2)
        OME2.IME2 <- sqrt(OME2 * IME2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- sqrt(OME2.IME2 * ROSE2.RISE2)
        
        if (length(step1) == 6) {
          REV <- sum(Y.ini[, dmu] * P1[, dmu])
          COST <- sum(X.ini[, dmu] * W1[, dmu])
          PROF <- REV/COST
          P <- REV/AO
          W <- COST/AI
          TT <- P/W
          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, 
          MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, 
          OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
          TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2, 
          RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, 
          OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, 
          Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, 
          OSME2.ISME2 = OSME2.ISME2, RME2 = RME2, PRICESI.ms, PRICESO.ms, PRICESI.nt, PRICESO.nt)
        }
      }
    }
    return(res1)
  }
  res2
}

### Hicks-Moorsteen, print fonction
print.HicksMoorsteen <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nHicks-Moorsteen productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]], digits = digits), digits = digits)
  cat("\n\nHicks-Moorsteen productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]], digits = digits), digits = digits)
  cat("\n\nHicks-Moorsteen productivity shadow prices:\n\n")
  print(x[["Shadowp"]], digits = digits)
  cat("\n")
  invisible(x)
}
