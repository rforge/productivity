## Laspeyres with technical change

las.1 <- function(data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled) {
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
  P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  P2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$p.vars]))
  }
  W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  W2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$w.vars]))
  }
  
  ## unscaled data
  if (scaled == TRUE) {
    Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
  } else {
    Y.ini <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
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
    Qt <- sum(P2[, dmu] * Y1[, dmu])
    Qs <- sum(P2[, dmu] * Y2[, dmu])
    Xt <- sum(W2[, dmu] * X1[, dmu])
    Xs <- sum(W2[, dmu] * X2[, dmu])
    
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P2[, 
      dmu], PRICESI = W2[, dmu], rts)
    TFPE <- TFP/MP
    
    TFP2 <- Qs/Xs
    MP2 <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P2[, 
      dmu], PRICESI = W2[, dmu], rts)
    TFPE2 <- TFP2/MP2

      PRICESO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      PRICESI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
    
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
      RE <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P2[, 
        dmu], rts)
      RAE <- RE/OTE
      ROSE <- ((AO/(OTE * RAE))/AI)/MP
      OSME <- RAE * ROSE
      RME <- TFPE/OTE/OSE
      REV <- sum(Y.ini[, dmu] * P2[, dmu])
      COST <- sum(X.ini[, dmu] * W2[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      
      OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
      OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
      RE2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P2[, 
        dmu], rts)
      RAE2 <- RE2/OTE2
      ROSE2 <- ((Qs/(OTE2 * RAE2))/Xs)/MP2
      OSME2 <- RAE2 * ROSE2
      RME2 <- TFPE2/OTE2/OSE2
      
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
        TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, RAE = RAE, ROSE = ROSE, OSME = OSME, 
        RME = RME, RE = RE, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
        TFPE2 = TFPE2, RAE2 = RAE2, ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2, RE2 = RE2, 
        PRICESI, PRICESO)
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          rts = "crs"))/ITE
        CE <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          PRICESI = W2[, dmu], rts))
        CAE <- CE/ITE
        RISE <- (AO/(AI * CAE * ITE))/MP
        ISME <- CAE * RISE
        RME <- TFPE/ITE/ISE
        REV <- sum(Y.ini[, dmu] * P2[, dmu])
        COST <- sum(X.ini[, dmu] * W2[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
          rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
          rts = "crs"))/ITE2
        CE2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
          PRICESI = W2[, dmu], rts))
        CAE2 <- CE2/ITE2
        RISE2 <- (Qs/(Xs * CAE2 * ITE2))/MP2
        ISME2 <- CAE2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
          TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, CAE = CAE, RISE = RISE, 
          ISME = ISME, RME = RME, CE = CE, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, 
          MP2 = MP2, TFPE2 = TFPE2, CAE2 = CAE2, RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2, 
          CE2 = CE2, PRICESI, PRICESO)
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
        RE <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = P2[, 
          dmu], rts)
        RAE <- RE/OTE
        ROSE <- ((AO/(OTE * RAE))/AI)/MP
        OSME <- RAE * ROSE
        RME <- TFPE/OTE/OSE
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          rts = "crs"))/ITE
        CE <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, 
          PRICESI = W2[, dmu], rts))
        CAE <- CE/ITE
        RISE <- (AO/(AI * CAE * ITE))/MP
        ISME <- CAE * RISE
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        RAE.CAE <- sqrt(RAE * CAE)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- sqrt(RAE.CAE * ROSE.RISE)
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y.ini[, dmu] * P2[, dmu])
        COST <- sum(X.ini[, dmu] * W2[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        
        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
        RE2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = P2[, 
          dmu], rts)
        RAE2 <- RE2/OTE2
        ROSE2 <- ((Qs/(OTE2 * RAE2))/Xs)/MP2
        OSME2 <- RAE2 * ROSE2
        RME2 <- TFPE2/OTE2/OSE2
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
          rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
          rts = "crs"))/ITE2
        CE2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, 
          PRICESI = W2[, dmu], rts))
        CAE2 <- CE2/ITE2
        RISE2 <- (Qs/(Xs * CAE2 * ITE2))/MP2
        ISME2 <- CAE2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        OTE2.ITE2 <- sqrt(OTE2 * ITE2)
        OSE2.ISE2 <- sqrt(OSE2 * ISE2)
        RE2.CE2 <- sqrt(RE2 * CE2)
        RAE2.CAE2 <- sqrt(RAE2 * CAE2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- sqrt(RAE2.CAE2 * ROSE2.RISE2)
        
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
          TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, RAE.CAE = RAE.CAE, 
          ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, RE.CE = RE.CE, Qt = Qt, 
          Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, RAE2.CAE2 = RAE2.CAE2, 
          ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2, RME2 = RME2, RE2.CE2 = RE2.CE2, 
          PRICESI, PRICESO)
      }
    }
    return(res1)
  }
  res2
}

## Laspeyres without technical change

las.2 <- function(data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled) {
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
  P1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  P2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$p.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$p.vars]))
  }
  W1 <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  W2 <- if (ano == 1) {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$w.vars]))
  } else {
    t(as.matrix(data[data[, step1$time.var] == year.vec[ano - 1], step1$w.vars]))
  }
  
  ## unscaled data
  if (scaled == TRUE) {
    Y.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data.in[data.in[, step1$time.var] == year.vec[ano], step1$x.vars]))
  } else {
    Y.ini <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$y.vars]))
    X.ini <- t(as.matrix(data[data[, step1$time.var] == year.vec[ano], step1$x.vars]))
  }
  
  XREFs <- t(as.matrix(data[, step1$x.vars]))
  YREFs <- t(as.matrix(data[, step1$y.vars]))
  
  res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), 
    .combine = rbind, .packages = c("Rglpk")) %dopar% {
      if (parallel == FALSE) {
        cat("\r")
        cat('Progress:', ano/length(year.vec)*100, '%', '\r')
        flush.console()
        if(ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == 
        year.vec[ano], step1$id.var])) cat('DONE!          \n\r')
      }
    Qt <- sum(P2[, dmu] * Y1[, dmu])
    Qs <- sum(P2[, dmu] * Y2[, dmu])
    Xt <- sum(W2[, dmu] * X1[, dmu])
    Xs <- sum(W2[, dmu] * X2[, dmu])
    
    AO <- Qt
    AI <- Xt
    TFP <- AO/AI
    MP <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, 
      dmu], PRICESI = W2[, dmu], rts)
    TFPE <- TFP/MP
    
    TFP2 <- Qs/Xs
    MP2 <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, 
      dmu], PRICESI = W2[, dmu], rts)
    TFPE2 <- TFP2/MP2
    
      PRICESO <- DO.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
      PRICESI <- DI.shdu(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
    
    if (orientation == "out") {
      OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
      OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
      RE <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, 
        dmu], rts)
      RAE <- RE/OTE
      ROSE <- ((AO/(OTE * RAE))/AI)/MP
      OSME <- RAE * ROSE
      RME <- TFPE/OTE/OSE
      REV <- sum(Y.ini[, dmu] * P2[, dmu])
      COST <- sum(X.ini[, dmu] * W2[, dmu])
      PROF <- REV/COST
      P <- REV/AO
      W <- COST/AI
      TT <- P/W
      
      OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
      OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
      RE2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, 
        dmu], rts)
      RAE2 <- RE2/OTE2
      ROSE2 <- ((Qs/(OTE2 * RAE2))/Xs)/MP2
      OSME2 <- RAE2 * ROSE2
      RME2 <- TFPE2/OTE2/OSE2
      
      res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
        TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, RAE = RAE, ROSE = ROSE, OSME = OSME, 
        RME = RME, RE = RE, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
        TFPE2 = TFPE2, RAE2 = RAE2, ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2, RE2 = RE2, 
        PRICESI, PRICESO)
    } else {
      if (orientation == "in") {
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, 
          rts = "crs"))/ITE
        CE <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, 
          PRICESI = W2[, dmu], rts))
        CAE <- CE/ITE
        RISE <- (AO/(AI * CAE * ITE))/MP
        ISME <- CAE * RISE
        RME <- TFPE/ITE/ISE
        REV <- sum(Y.ini[, dmu] * P2[, dmu])
        COST <- sum(X.ini[, dmu] * W2[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
          rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
          rts = "crs"))/ITE2
        CE2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
          PRICESI = W2[, dmu], rts))
        CAE2 <- CE2/ITE2
        RISE2 <- (Qs/(Xs * CAE2 * ITE2))/MP2
        ISME2 <- CAE2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
          TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, CAE = CAE, RISE = RISE, 
          ISME = ISME, RME = RME, CE = CE, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, 
          MP2 = MP2, TFPE2 = TFPE2, CAE2 = CAE2, RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2, 
          CE2 = CE2, PRICESI, PRICESO)
      } else {
        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
        RE <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, 
          dmu], rts)
        RAE <- RE/OTE
        ROSE <- ((AO/(OTE * RAE))/AI)/MP
        OSME <- RAE * ROSE
        RME <- TFPE/OTE/OSE
        ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
        ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, 
          rts = "crs"))/ITE
        CE <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, 
          PRICESI = W2[, dmu], rts))
        CAE <- CE/ITE
        RISE <- (AO/(AI * CAE * ITE))/MP
        ISME <- CAE * RISE
        OTE.ITE <- sqrt(OTE * ITE)
        OSE.ISE <- sqrt(OSE * ISE)
        RAE.CAE <- sqrt(RAE * CAE)
        ROSE.RISE <- sqrt(ROSE * RISE)
        OSME.ISME <- sqrt(RAE.CAE * ROSE.RISE)
        RE.CE <- sqrt(RE * CE)
        REV <- sum(Y.ini[, dmu] * P2[, dmu])
        COST <- sum(X.ini[, dmu] * W2[, dmu])
        PROF <- REV/COST
        P <- REV/AO
        W <- COST/AI
        TT <- P/W
        
        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
        RE2 <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = P2[, 
          dmu], rts)
        RAE2 <- RE2/OTE2
        ROSE2 <- ((Qs/(OTE2 * RAE2))/Xs)/MP2
        OSME2 <- RAE2 * ROSE2
        RME2 <- TFPE2/OTE2/OSE2
        
        ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
          rts)
        ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
          rts = "crs"))/ITE2
        CE2 <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, 
          PRICESI = W2[, dmu], rts))
        CAE2 <- CE2/ITE2
        RISE2 <- (Qs/(Xs * CAE2 * ITE2))/MP2
        ISME2 <- CAE2 * RISE2
        RME2 <- TFPE2/ITE2/ISE2
        
        OTE2.ITE2 <- sqrt(OTE2 * ITE2)
        OSE2.ISE2 <- sqrt(OSE2 * ISE2)
        RE2.CE2 <- sqrt(RE2 * CE2)
        RAE2.CAE2 <- sqrt(RAE2 * CAE2)
        ROSE2.RISE2 <- sqrt(ROSE2 * RISE2)
        OSME2.ISME2 <- sqrt(RAE2.CAE2 * ROSE2.RISE2)
        
        
        res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, 
          TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, RAE.CAE = RAE.CAE, 
          ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, RE.CE = RE.CE, Qt = Qt, 
          Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, RAE2.CAE2 = RAE2.CAE2, 
          ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2, RME2 = RME2, RE2.CE2 = RE2.CE2, 
          PRICESI, PRICESO)
      }
    }
    return(res1)
  }
  res2
}

### Laspeyres, print fonction

print.Laspeyres <- function(x, digits = NULL, ...) {
    if (is.null(digits)) {
        digits <- max(3, getOption("digits") - 3)
    }
    cat("\nLaspeyres productivity and profitability levels (summary):\n\n")
    print(summary(x[["Levels"]], digits = digits), digits = digits)
    cat("\n\nLaspeyres productivity and profitability changes (summary):\n\n")
    print(summary(x[["Changes"]], digits = digits), digits = digits)
    cat("\n\nLaspeyres productivity shadow prices (summary):\n\n")
    print(summary(x[["Shadowp"]], digits = digits), digits = digits)
    cat("\n")
    invisible(x)
}
