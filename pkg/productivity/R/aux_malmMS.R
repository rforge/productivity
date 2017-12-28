## Malmquist-ms with technical change

malm.ms.1 <- function(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled) {
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
    
    PRICESO <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts)
    PRICESI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
    
    Qt <- sum(PRICESO * Y1[, dmu])
    Qs <- sum(PRICESO * Y2[, dmu])
    Xt <- sum(PRICESI * X1[, dmu])
    Xs <- sum(PRICESI * X2[, dmu])
    
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    
    TFP2 <- Qs/Xs
    MP2 <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO, PRICESI, rts)
    TFPE2 <- TFP2/MP2
    
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      # OSME <- OSE * RME
      
      OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
      OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
      OME2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO, rts)/OTE2
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
          OSME2 = OSME2, RME2 = RME2, PRICESI, PRICESO)
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, 
          OME2 = OME2, ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2, PRICESI, PRICESO)
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2
        IME2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI, rts))/ITE2
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
          ISME2 = ISME2, RME2 = RME2, PRICESI, PRICESO)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, 
          RISE = RISE, ISME = ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
          TFPE2 = TFPE2, IME2 = IME2, RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2, PRICESI, PRICESO)
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO, rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        
        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
        OME2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO, rts)/OTE2
        ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
        OSME2 <- OME2 * ROSE2
        RME2 <- TFPE2/OTE2/OSE2
        
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        # RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2
        IME2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI, rts))/ITE2
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
          RME2 = RME2, PRICESI, PRICESO)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, 
          OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, 
          Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, 
          OSME2.ISME2 = OSME2.ISME2, RME2 = RME2, PRICESI, PRICESO)
        }
      }
    }
    return(res1)
  }
  res2
}

## Malmquist-ms without technical change

malm.ms.2 <- function(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled) {
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
    
    PRICESO <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
    PRICESI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
    
    Qt <- sum(PRICESO * Y1[, dmu])
    Qs <- sum(PRICESO * Y2[, dmu])
    Xt <- sum(PRICESI * X1[, dmu])
    Xs <- sum(PRICESI * X2[, dmu])
    
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO, PRICESI, rts)
    TFPE <- TFP/MP
    
    TFP2 <- Qs/Xs
    MP2 <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO, PRICESI, rts)
    TFPE2 <- TFP2/MP2
    
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
      OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO, rts)/OTE
      ROSE <- ((AO/(OTE * OME))/AI)/MP
      OSME <- OME * ROSE
      RME <- TFPE/OTE/OSE
      # OSME <- OSE * RME
      
      OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
      OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
      OME2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO, rts)/OTE2
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
          OSME2 = OSME2, RME2 = RME2, PRICESI, PRICESO)
      } else {
        res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
          OSME = OSME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, 
          OME2 = OME2, ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2, PRICESI, PRICESO)
      }
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2
        IME2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI, rts))/ITE2
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
          ISME2 = ISME2, RME2 = RME2, PRICESI, PRICESO)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, 
          RISE = RISE, ISME = ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
          TFPE2 = TFPE2, IME2 = IME2, RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2, PRICESI, PRICESO)
        }
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
        OME <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO, rts)/OTE
        ROSE <- ((AO/(OTE * OME))/AI)/MP
        OSME <- OME * ROSE
        RME <- TFPE/OTE/OSE
        # OSME <- OSE * RME
        
        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
        OME2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO, rts)/OTE2
        ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
        OSME2 <- OME2 * ROSE2
        RME2 <- TFPE2/OTE2/OSE2
        
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE
        IME <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI, rts))/ITE
        RISE <- (AO/(AI * IME * ITE))/MP
        ISME <- IME * RISE
        # RME <- TFPE/ITE/ISE
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2
        IME2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI, rts))/ITE2
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
          RME2 = RME2, PRICESI, PRICESO)
        } else {
          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, 
          OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, 
          Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, 
          OSME2.ISME2 = OSME2.ISME2, RME2 = RME2, PRICESI, PRICESO)
        }
      }
    }
    return(res1)
  }
  res2
}

### Malmquist - MS, print fonction
print.MalmquistMS <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  cat("\nMalmquist-MS productivity and profitability levels (summary):\n\n")
  print(summary(x[["Levels"]], digits = digits), digits = digits)
  cat("\n\nMalmquist-MS productivity and profitability changes (summary):\n\n")
  print(summary(x[["Changes"]], digits = digits), digits = digits)
  cat("\n\nMalmquist-MS productivity shadow prices:\n\n")
  print(x[["Shadowp"]], digits = digits)
  cat("\n")
  invisible(x)
}
