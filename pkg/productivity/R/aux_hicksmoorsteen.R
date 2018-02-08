## Hicks-Moorsteen with technical change (define malm-hs and malm-it)

hm.1 <- function (data, data.in, step1, ano, year.vec, tech.reg, rts, orientation, parallel, scaled, 
                  components) {
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
  if (components == FALSE) {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (parallel == FALSE) {
                        cat("\r")
                        cat("Progress:", round.up(ano/length(year.vec) * 100, 0), "%", "\r")
                        flush.console()
                        if (ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == year.vec[ano], step1$id.var])) 
                          cat("Hicks-Moorsteen DONE!               \n\r")
                      }
                      
                      PRICESO.hs <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESI.hs <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESO.it <- DO.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      PRICESI.it <- DI.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      
                      Qt <- sqrt(sum(PRICESO.hs * Y1[, dmu]) * sum(PRICESO.it * Y1[, dmu]))
                      Xt <- sqrt(sum(PRICESI.hs * X1[, dmu]) * sum(PRICESI.it * X1[, dmu]))
                      Qs <- sqrt(sum(PRICESO.hs * Y2[, dmu]) * sum(PRICESO.it * Y2[, dmu]))
                      Xs <- sqrt(sum(PRICESI.hs * X2[, dmu]) * sum(PRICESI.it * X2[, dmu]))
                      
                      AO <- Qt
                      AI <- Xt
                      TFP <- AO/AI
                      MP <- sqrt(D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                   D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE <- TFP/MP
                      
                      TFP2 <- Qs/Xs
                      MP2 <- sqrt(D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                    D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE2 <- TFP2/MP2
                      
                      if (orientation == "out") {
                        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
                        OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts) * 
                                      DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts))/OTE
                        ROSE <- ((AO/(OTE * OME))/AI)/MP
                        OSME <- OME * ROSE
                        RME <- TFPE/OTE/OSE
                        # OSME <- OSE * RME
                        
                        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
                        OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts) * 
                                       DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts))/OTE2
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
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                    TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME,
                                    Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = OME2, 
                                    ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2) 
                        } else {
                          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
                                    OSME = OSME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = OME2, 
                                    ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2) 
                        }
                      } else {
                        if (orientation == "in") {
                          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
                          IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts)) * 
                                        (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI =  PRICESI.it, rts)))/ITE
                          RISE <- (AO/(AI * IME * ITE))/MP
                          ISME <- IME * RISE
                          RME <- TFPE/ITE/ISE
                          
                          ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2
                          IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts)) * 
                                         (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts)))/ITE2
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
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME,
                                      Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = IME2, 
                                      RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2)
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, 
                                      ISME = ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = IME2, 
                                      RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2) 
                          }
                        } else {
                          OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE
                          OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts) * 
                                        DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts))/OTE
                          ROSE <- ((AO/(OTE * OME))/AI)/MP
                          OSME <- OME * ROSE
                          RME <- TFPE/OTE/OSE
                          # OSME <- OSE * RME
                          
                          OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2
                          OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts) * 
                                         DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts))/OTE2
                          ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
                          OSME2 <- OME2 * ROSE2
                          RME2 <- TFPE2/OTE2/OSE2
                          
                          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE
                          IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts)) * 
                                        (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI =  PRICESI.it, rts)))/ITE
                          RISE <- (AO/(AI * IME * ITE))/MP
                          ISME <- IME * RISE
                          # RME <- TFPE/ITE/ISE
                          
                          ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2
                          IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts)) * 
                                         (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts)))/ITE2
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
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
                                      RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2,
                                      RME2 = RME2)
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, 
                                      ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs,
                                      TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2,
                                      RME2 = RME2)
                          }
                        }
                      }
                      return(res1)
                    }
  } else {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (parallel == FALSE) {
                        cat("\r")
                        cat("Progress:", round.up(ano/length(year.vec) * 100, 0), "%", "\r")
                        flush.console()
                        if (ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == year.vec[ano], step1$id.var])) 
                          cat("Hicks-Moorsteen DONE!               \n\r")
                      }
                      
                      PRICESO.hs <- DO.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESI.hs <- DI.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                      PRICESO.it <- DO.shdu(XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      PRICESI.it <- DI.shdu(XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                      
                      Qt.hs <- sum(PRICESO.hs * Y1[, dmu])
                      Qs.hs <- sum(PRICESO.hs * Y2[, dmu])
                      Xt.hs <- sum(PRICESI.hs * X1[, dmu])
                      Xs.hs <- sum(PRICESI.hs * X2[, dmu])
                      
                      Qt.it <- sum(PRICESO.it * Y1[, dmu])
                      Qs.it <- sum(PRICESO.it * Y2[, dmu])
                      Xt.it <- sum(PRICESI.it * X1[, dmu])
                      Xs.it <- sum(PRICESI.it * X2[, dmu])
                      
                      AO.hs <- Qt.hs
                      AI.hs <- Xt.hs
                      TFP.hs <- AO.hs/AI.hs
                      MP.hs <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE.hs <- TFP.hs/MP.hs
                      
                      TFP2.hs <- Qs.hs/Xs.hs
                      MP2.hs <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE2.hs <- TFP2.hs/MP2.hs
                      
                      AO.it <- Qt.it
                      AI.it <- Xt.it
                      TFP.it <- AO.it/AI.it
                      MP.it <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE.it <- TFP.it/MP.it
                      
                      TFP2.it <- Qs.it/Xs.it
                      MP2.it <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE2.it <- TFP2.it/MP2.it
                      
                      if (orientation == "out") {
                        OTE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                        OSE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE.hs
                        OME.hs <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts)/OTE.hs
                        ROSE.hs <- ((AO.hs/(OTE.hs * OME.hs))/AI.hs)/MP.hs
                        OSME.hs <- OME.hs * ROSE.hs
                        RME.hs <- TFPE.hs/OTE.hs/OSE.hs
                        # OSME <- OSE * RME
                        
                        OTE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                        OSE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2.hs
                        OME2.hs <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts)/OTE2.hs
                        ROSE2.hs <- ((Qs.hs/(OTE2.hs * OME2.hs))/Xs.hs)/MP2.hs
                        OSME2.hs <- OME2.hs * ROSE2.hs
                        RME2.hs <- TFPE2.hs/OTE2.hs/OSE2.hs
                        
                        OTE.it <- OTE.hs
                        OSE.it <- OSE.hs
                        OME.it <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts)/OTE.it
                        ROSE.it <- ((AO.it/(OTE.it * OME.it))/AI.it)/MP.it
                        OSME.it <- OME.it * ROSE.it
                        RME.it <- TFPE.it/OTE.it/OSE.it
                        # OSME <- OSE * RME
                        
                        OTE2.it <- OTE2.hs
                        OSE2.it <- OSE2.hs
                        OME2.it <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts)/OTE2.it
                        ROSE2.it <- ((Qs.it/(OTE2.it * OME2.it))/Xs.it)/MP2.it
                        OSME2.it <- OME2.it * ROSE2.it
                        RME2.it <- TFPE2.it/OTE2.it/OSE2.it
                        
                        if (length(step1) == 6) {
                          REV <- sum(Y.ini[, dmu] * P1[, dmu])
                          COST <- sum(X.ini[, dmu] * W1[, dmu])
                          PROF <- REV/COST
                          P.hs <- REV/AO.hs
                          W.hs <- COST/AI.hs
                          TT.hs <- P.hs/W.hs
                          P.it <- REV/AO.it
                          W.it <- COST/AI.it
                          TT.it <- P.it/W.it
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                    TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.hs = OTE.hs, OSE.hs = OSE.hs, 
                                    OME.hs = OME.hs, ROSE.hs = ROSE.hs, OSME.hs = OSME.hs, RME.hs = RME.hs,
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, OME2.hs = OME2.hs, ROSE2.hs = ROSE2.hs, OSME2.hs = OSME2.hs,
                                    RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.it = OTE.it, OSE.it = OSE.it, 
                                    OME.it = OME.it, ROSE.it = ROSE.it, OSME.it = OSME.it, RME.it = RME.it,
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, OME2.it = OME2.it, ROSE2.it = ROSE2.it, OSME2.it = OSME2.it,
                                    RME2.it = RME2.it, PRICESI.it, PRICESO.it) 
                        } else {
                          res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.hs = OTE.hs, OSE.hs = OSE.hs, 
                                    OME.hs = OME.hs, ROSE.hs = ROSE.hs, OSME.hs = OSME.hs, RME.hs = RME.hs,
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, OME2.hs = OME2.hs, ROSE2.hs = ROSE2.hs, OSME2.hs = OSME2.hs,
                                    RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.it = OTE.it, OSE.it = OSE.it, 
                                    OME.it = OME.it, ROSE.it = ROSE.it, OSME.it = OSME.it, RME.it = RME.it,
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, OME2.it = OME2.it, ROSE2.it = ROSE2.it, OSME2.it = OSME2.it,
                                    RME2.it = RME2.it, PRICESI.it, PRICESO.it) 
                        }
                      } else {
                        if (orientation == "in") {
                          ITE.hs <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          ISE.hs <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE.hs
                          IME.hs <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts))/ITE.hs
                          RISE.hs <- (AO.hs/(AI.hs * IME.hs * ITE.hs))/MP.hs
                          ISME.hs <- IME.hs * RISE.hs
                          RME.hs <- TFPE.hs/ITE.hs/ISE.hs
                          
                          ITE2.hs <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          ISE2.hs <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2.hs
                          IME2.hs <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts))/ITE2.hs
                          RISE2.hs <- (Qs.hs/(Xs.hs * IME2.hs * ITE2.hs))/MP2.hs
                          ISME2.hs <- IME2.hs * RISE2.hs
                          RME2.hs <- TFPE2.hs/ITE2.hs/ISE2.hs
                          
                          ITE.it <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          ISE.it <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE.it
                          IME.it <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.it, rts))/ITE.it
                          RISE.it <- (AO.it/(AI.it * IME.it * ITE.it))/MP.it
                          ISME.it <- IME.it * RISE.it
                          RME.it <- TFPE.it/ITE.it/ISE.it
                          
                          ITE2.it <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          ISE2.it <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2.it
                          IME2.it <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts))/ITE2.it
                          RISE2.it <- (Qs.it/(Xs.it * IME2.it * ITE2.it))/MP2.it
                          ISME2.it <- IME2.it * RISE2.it
                          RME2.it <- TFPE2.it/ITE2.it/ISE2.it
                          
                          if (length(step1) == 6) {
                            REV <- sum(Y.ini[, dmu] * P1[, dmu])
                            COST <- sum(X.ini[, dmu] * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, ITE.hs = ITE.hs, ISE.hs = ISE.hs, 
                                      IME.hs = IME.hs, RISE.hs = RISE.hs, ISME.hs = ISME.hs, RME.hs = RME.hs,
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, IME2.hs = IME2.hs, RISE2.hs = RISE2.hs, ISME2.hs = ISME2.hs,
                                      RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, ITE.it = ITE.it, ISE.it = ISE.it, 
                                      IME.it = IME.it, RISE.it = RISE.it, ISME.it = ISME.it, RME.it = RME.it,
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, IME2.it = IME2.it, RISE2.it = RISE2.it, ISME2.it = ISME2.it,
                                      RME2.it = RME2.it, PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, ITE.hs = ITE.hs, ISE.hs = ISE.hs, 
                                      IME.hs = IME.hs, RISE.hs = RISE.hs, ISME.hs = ISME.hs, RME.hs = RME.hs,
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, IME2.hs = IME2.hs, RISE2.hs = RISE2.hs, ISME2.hs = ISME2.hs,
                                      RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, ITE.it = ITE.it, ISE.it = ISE.it, 
                                      IME.it = IME.it, RISE.it = RISE.it, ISME.it = ISME.it, RME.it = RME.it,
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, IME2.it = IME2.it, RISE2.it = RISE2.it, ISME2.it = ISME2.it,
                                      RME2.it = RME2.it, PRICESI.it, PRICESO.it) 
                          }
                        } else {
                          OTE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          OSE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs")/OTE.hs
                          OME.hs <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.hs, rts)/OTE.hs
                          ROSE.hs <- ((AO.hs/(OTE.hs * OME.hs))/AI.hs)/MP.hs
                          OSME.hs <- OME.hs * ROSE.hs
                          RME.hs <- TFPE.hs/OTE.hs/OSE.hs
                          # OSME <- OSE * RME
                          
                          OTE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          OSE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs")/OTE2.hs
                          OME2.hs <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.hs, rts)/OTE2.hs
                          ROSE2.hs <- ((Qs.hs/(OTE2.hs * OME2.hs))/Xs.hs)/MP2.hs
                          OSME2.hs <- OME2.hs * ROSE2.hs
                          RME2.hs <- TFPE2.hs/OTE2.hs/OSE2.hs
                          
                          OTE.it <- OTE.hs
                          OSE.it <- OSE.hs
                          OME.it <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESO = PRICESO.it, rts)/OTE.it
                          ROSE.it <- ((AO.it/(OTE.it * OME.it))/AI.it)/MP.it
                          OSME.it <- OME.it * ROSE.it
                          RME.it <- TFPE.it/OTE.it/OSE.it
                          # OSME <- OSE * RME
                          
                          OTE2.it <- OTE2.hs
                          OSE2.it <- OSE2.hs
                          OME2.it <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESO = PRICESO.it, rts)/OTE2.it
                          ROSE2.it <- ((Qs.it/(OTE2.it * OME2.it))/Xs.it)/MP2.it
                          OSME2.it <- OME2.it * ROSE2.it
                          RME2.it <- TFPE2.it/OTE2.it/OSE2.it
                          
                          ITE.hs <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          ISE.hs <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE.hs
                          IME.hs <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.hs, rts))/ITE.hs
                          RISE.hs <- (AO.hs/(AI.hs * IME.hs * ITE.hs))/MP.hs
                          ISME.hs <- IME.hs * RISE.hs
                          RME.hs <- TFPE.hs/ITE.hs/ISE.hs
                          
                          ITE2.hs <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          ISE2.hs <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2.hs
                          IME2.hs <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.hs, rts))/ITE2.hs
                          RISE2.hs <- (Qs.hs/(Xs.hs * IME2.hs * ITE2.hs))/MP2.hs
                          ISME2.hs <- IME2.hs * RISE2.hs
                          RME2.hs <- TFPE2.hs/ITE2.hs/ISE2.hs
                          
                          ITE.it <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts)
                          ISE.it <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, rts = "crs"))/ITE.it
                          IME.it <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREF1, YREF = YREF1, PRICESI = PRICESI.it, rts))/ITE.it
                          RISE.it <- (AO.it/(AI.it * IME.it * ITE.it))/MP.it
                          ISME.it <- IME.it * RISE.it
                          RME.it <- TFPE.it/ITE.it/ISE.it
                          
                          ITE2.it <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts)
                          ISE2.it <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, rts = "crs"))/ITE2.it
                          IME2.it <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREF2, YREF = YREF2, PRICESI = PRICESI.it, rts))/ITE2.it
                          RISE2.it <- (Qs.it/(Xs.it * IME2.it * ITE2.it))/MP2.it
                          ISME2.it <- IME2.it * RISE2.it
                          RME2.it <- TFPE2.it/ITE2.it/ISE2.it
                          
                          OTE.ITE.hs <- sqrt(OTE.hs * ITE.hs)
                          OSE.ISE.hs <- sqrt(OSE.hs * ISE.hs)
                          OME.IME.hs <- sqrt(OME.hs * IME.hs)
                          ROSE.RISE.hs <- sqrt(ROSE.hs * RISE)
                          OSME.ISME.hs <- sqrt(OME.IME.hs * ROSE.RISE.hs)
                          # RME <- TFPE/OTE.ITE/OSE.ISE
                          
                          OTE2.ITE2.hs <- sqrt(OTE2.hs * ITE2.hs)
                          OSE2.ISE2.hs <- sqrt(OSE2.hs * ISE2.hs)
                          OME2.IME2.hs <- sqrt(OME2.hs * IME2.hs)
                          ROSE2.RISE2.hs <- sqrt(ROSE2.hs * RISE2.hs)
                          OSME2.ISME2.hs <- sqrt(OME2.IME2.hs * ROSE2.RISE2.hs)
                          
                          OTE.ITE.it <- sqrt(OTE.it * ITE.it)
                          OSE.ISE.it <- sqrt(OSE.it * ISE.it)
                          OME.IME.it <- sqrt(OME.it * IME.it)
                          ROSE.RISE.it <- sqrt(ROSE.it * RISE)
                          OSME.ISME.it <- sqrt(OME.IME.it * ROSE.RISE.it)
                          # RME <- TFPE/OTE.ITE/OSE.ISE
                          
                          OTE2.ITE2.it <- sqrt(OTE2.it * ITE2.it)
                          OSE2.ISE2.it <- sqrt(OSE2.it * ISE2.it)
                          OME2.IME2.it <- sqrt(OME2.it * IME2.it)
                          ROSE2.RISE2.it <- sqrt(ROSE2.it * RISE2.it)
                          OSME2.ISME2.it <- sqrt(OME2.IME2.it * ROSE2.RISE2.it)
                          
                          if (length(step1) == 6) {
                            REV <- sum(Y.ini[, dmu] * P1[, dmu])
                            COST <- sum(X.ini[, dmu] * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.ITE.hs = OTE.ITE.hs, OSE.ISE.hs = OSE.ISE.hs,
                                      OME.IME.hs = OME.IME.hs, ROSE.RISE.hs = ROSE.RISE.hs, OSME.ISME.hs = OSME.ISME.hs, 
                                      RME.hs = RME.hs, Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = OME2.IME2.hs,
                                      ROSE2.RISE2.hs = ROSE2.RISE2.hs, OSME2.ISME2.hs = OSME2.ISME2.hs, RME2.hs = RME2.hs, 
                                      PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.ITE.it = OTE.ITE.it, OSE.ISE.it = OSE.ISE.it,
                                      OME.IME.it = OME.IME.it, ROSE.RISE.it = ROSE.RISE.it, OSME.ISME.it = OSME.ISME.it, 
                                      RME.it = RME.it, Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = OME2.IME2.it,
                                      ROSE2.RISE2.it = ROSE2.RISE2.it, OSME2.ISME2.it = OSME2.ISME2.it, RME2.it = RME2.it, 
                                      PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.ITE.hs = OTE.ITE.hs, OSE.ISE.hs = OSE.ISE.hs,
                                      OME.IME.hs = OME.IME.hs, ROSE.RISE.hs = ROSE.RISE.hs, OSME.ISME.hs = OSME.ISME.hs, 
                                      RME.hs = RME.hs, Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = OME2.IME2.hs,
                                      ROSE2.RISE2.hs = ROSE2.RISE2.hs, OSME2.ISME2.hs = OSME2.ISME2.hs, RME2.hs = RME2.hs, 
                                      PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.ITE.it = OTE.ITE.it, OSE.ISE.it = OSE.ISE.it,
                                      OME.IME.it = OME.IME.it, ROSE.RISE.it = ROSE.RISE.it, OSME.ISME.it = OSME.ISME.it, 
                                      RME.it = RME.it, Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = OME2.IME2.it,
                                      ROSE2.RISE2.it = ROSE2.RISE2.it, OSME2.ISME2.it = OSME2.ISME2.it, RME2.it = RME2.it, 
                                      PRICESI.it, PRICESO.it)
                          }
                        }
                      }
                      return(res1)
                    }
  }
  res2
}

## Hicks-Moorsteen without technical change

hm.2 <- function (data, data.in, step1, ano, year.vec, rts, orientation, parallel, scaled,
                  components) {
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
  
  ## all periods reference
  XREFs <- t(as.matrix(data[, step1$x.vars]))
  YREFs <- t(as.matrix(data[, step1$y.vars]))
  
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
  
  if (components == FALSE) {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (parallel == FALSE) {
                        cat("\r")
                        cat("Progress:", round.up(ano/length(year.vec) * 100, 0), "%", "\r")
                        flush.console()
                        if (ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == year.vec[ano], step1$id.var])) 
                          cat("Hicks-Moosteen DONE!               \n\r")
                      }
                      
                      PRICESO.hs <- DO.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.hs <- DI.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESO.it <- DO.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.it <- DI.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      
                      Qt <- sqrt(sum(PRICESO.hs * Y1[, dmu]) * sum(PRICESO.it * Y1[, dmu]))
                      Xt <- sqrt(sum(PRICESI.hs * X1[, dmu]) * sum(PRICESI.it * X1[, dmu]))
                      Qs <- sqrt(sum(PRICESO.hs * Y2[, dmu]) * sum(PRICESO.it * Y2[, dmu]))
                      Xs <- sqrt(sum(PRICESI.hs * X2[,dmu]) * sum(PRICESI.it * X2[,dmu]))
                      
                      AO <- Qt
                      AI <- Xt
                      TFP <- AO / AI
                      MP <- sqrt(D.tfp (XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, 
                                        YREF = YREFs, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                   D.tfp (XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, 
                                          YREF = YREFs, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE <- TFP / MP
                      
                      TFP2 <- Qs / Xs
                      MP2 <- sqrt(D.tfp (XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF =YREFs, 
                                         PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts) * 
                                    D.tfp (XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF =YREFs, 
                                           PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts))
                      TFPE2 <- TFP2 / MP2
                      
                      if (orientation == "out") {
                        OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                        OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
                        OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts) * 
                                      DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts))/OTE
                        ROSE <- ((AO/(OTE * OME))/AI)/MP
                        OSME <- OME * ROSE
                        RME <- TFPE/OTE/OSE
                        # OSME <- OSE * RME
                        
                        OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                        OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
                        OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts) * 
                                       DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts))/OTE2
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
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                    TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, OSME = OSME, RME = RME,
                                    Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = OME2, 
                                    ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2) 
                        } else {
                          res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE = OTE, OSE = OSE, OME = OME, ROSE = ROSE, 
                                    OSME = OSME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, OME2 = OME2, 
                                    ROSE2 = ROSE2, OSME2 = OSME2, RME2 = RME2) 
                        }
                      } else {
                        if (orientation == "in") {
                          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE
                          IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)) * 
                                        (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI =  PRICESI.it, rts)))/ITE
                          RISE <- (AO/(AI * IME * ITE))/MP
                          ISME <- IME * RISE
                          RME <- TFPE/ITE/ISE
                          
                          ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2
                          IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)) * 
                                         (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)))/ITE2
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
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, ISME = ISME, RME = RME,
                                      Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = IME2, 
                                      RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2)
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, ITE = ITE, ISE = ISE, IME = IME, RISE = RISE, 
                                      ISME = ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, TFPE2 = TFPE2, IME2 = IME2, 
                                      RISE2 = RISE2, ISME2 = ISME2, RME2 = RME2) 
                          }
                        } else {
                          OTE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          OSE <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE
                          OME <- sqrt(DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts) * 
                                        DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts))/OTE
                          ROSE <- ((AO/(OTE * OME))/AI)/MP
                          OSME <- OME * ROSE
                          RME <- TFPE/OTE/OSE
                          # OSME <- OSE * RME
                          
                          OTE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          OSE2 <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2
                          OME2 <- sqrt(DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts) * 
                                         DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts))/OTE2
                          ROSE2 <- ((Qs/(OTE2 * OME2))/Xs)/MP2
                          OSME2 <- OME2 * ROSE2
                          RME2 <- TFPE2/OTE2/OSE2
                          
                          ITE <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE
                          IME <- sqrt((1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)) * 
                                        (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI =  PRICESI.it, rts)))/ITE
                          RISE <- (AO/(AI * IME * ITE))/MP
                          ISME <- IME * RISE
                          # RME <- TFPE/ITE/ISE
                          
                          ITE2 <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE2 <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2
                          IME2 <- sqrt((1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts)) * 
                                         (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts)))/ITE2
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
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P = P, W = W, TT = TT, AO = AO, AI = AI, TFP = TFP, MP = MP, 
                                      TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, 
                                      RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs, TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2,
                                      RME2 = RME2)
                          } else {
                            res1 <- c(AO = AO, AI = AI, TFP = TFP, MP = MP, TFPE = TFPE, OTE.ITE = OTE.ITE, OSE.ISE = OSE.ISE, OME.IME = OME.IME, 
                                      ROSE.RISE = ROSE.RISE, OSME.ISME = OSME.ISME, RME = RME, Qt = Qt, Qs = Qs, Xt = Xt, Xs = Xs,
                                      TFP2 = TFP2, MP2 = MP2, 
                                      TFPE2 = TFPE2, OME2.IME2 = OME2.IME2, ROSE2.RISE2 = ROSE2.RISE2, OSME2.ISME2 = OSME2.ISME2,
                                      RME2 = RME2)
                          }
                        }
                      }
                      return(res1)
                    }
  } else {
    res2 <- foreach(dmu = 1:length(data[data[, step1$time.var] == year.vec[ano], step1$id.var]), .combine = rbind, 
                    .packages = c("lpSolveAPI")) %dopar% {
                      if (parallel == FALSE) {
                        cat("\r")
                        cat("Progress:", round.up(ano/length(year.vec) * 100, 0), "%", "\r")
                        flush.console()
                        if (ano == length(year.vec) & dmu == length(data[data[, step1$time.var] == year.vec[ano], step1$id.var])) 
                          cat("Hicks-Moosteen DONE!               \n\r")
                      }
                      
                      PRICESO.hs <- DO.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.hs <- DI.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESO.it <- DO.shdu (XOBS = X1[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                      PRICESI.it <- DI.shdu (XOBS = X2[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)

                      Qt.hs <- sum(PRICESO.hs * Y1[, dmu])
                      Qs.hs <- sum(PRICESO.hs * Y2[, dmu])
                      Xt.hs <- sum(PRICESI.hs * X1[, dmu])
                      Xs.hs <- sum(PRICESI.hs * X2[,dmu])
                      
                      Qt.it <- sum(PRICESO.it * Y1[, dmu])
                      Qs.it <- sum(PRICESO.it * Y2[, dmu])
                      Xt.it <- sum(PRICESI.it * X1[, dmu])
                      Xs.it <- sum(PRICESI.it * X2[,dmu])
                      
                      AO.hs <- Qt.hs
                      AI.hs <- Xt.hs
                      TFP.hs <- AO.hs/AI.hs
                      MP.hs <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE.hs <- TFP.hs/MP.hs
                      
                      TFP2.hs <- Qs.hs/Xs.hs
                      MP2.hs <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, PRICESI = PRICESI.hs, rts)
                      TFPE2.hs <- TFP2.hs/MP2.hs
                      
                      AO.it <- Qt.it
                      AI.it <- Xt.it
                      TFP.it <- AO.it/AI.it
                      MP.it <- D.tfp(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE.it <- TFP.it/MP.it
                      
                      TFP2.it <- Qs.it/Xs.it
                      MP2.it <- D.tfp(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, PRICESI = PRICESI.it, rts)
                      TFPE2.it <- TFP2.it/MP2.it
                      
                      if (orientation == "out") {
                        OTE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                        OSE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE.hs
                        OME.hs <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)/OTE.hs
                        ROSE.hs <- ((AO.hs/(OTE.hs * OME.hs))/AI.hs)/MP.hs
                        OSME.hs <- OME.hs * ROSE.hs
                        RME.hs <- TFPE.hs/OTE.hs/OSE.hs
                        # OSME <- OSE * RME
                        
                        OTE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                        OSE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2.hs
                        OME2.hs <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)/OTE2.hs
                        ROSE2.hs <- ((Qs.hs/(OTE2.hs * OME2.hs))/Xs.hs)/MP2.hs
                        OSME2.hs <- OME2.hs * ROSE2.hs
                        RME2.hs <- TFPE2.hs/OTE2.hs/OSE2.hs
                        
                        OTE.it <- OTE.hs
                        OSE.it <- OSE.hs
                        OME.it <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)/OTE.it
                        ROSE.it <- ((AO.it/(OTE.it * OME.it))/AI.it)/MP.it
                        OSME.it <- OME.it * ROSE.it
                        RME.it <- TFPE.it/OTE.it/OSE.it
                        # OSME <- OSE * RME
                        
                        OTE2.it <- OTE2.hs
                        OSE2.it <- OSE2.hs
                        OME2.it <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)/OTE2.it
                        ROSE2.it <- ((Qs.it/(OTE2.it * OME2.it))/Xs.it)/MP2.it
                        OSME2.it <- OME2.it * ROSE2.it
                        RME2.it <- TFPE2.it/OTE2.it/OSE2.it
                        
                        if (length(step1) == 6) {
                          REV <- sum(Y.ini[, dmu] * P1[, dmu])
                          COST <- sum(X.ini[, dmu] * W1[, dmu])
                          PROF <- REV/COST
                          P.hs <- REV/AO.hs
                          W.hs <- COST/AI.hs
                          TT.hs <- P.hs/W.hs
                          P.it <- REV/AO.it
                          W.it <- COST/AI.it
                          TT.it <- P.it/W.it
                          res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                    TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.hs = OTE.hs, OSE.hs = OSE.hs, 
                                    OME.hs = OME.hs, ROSE.hs = ROSE.hs, OSME.hs = OSME.hs, RME.hs = RME.hs,
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, OME2.hs = OME2.hs, ROSE2.hs = ROSE2.hs, OSME2.hs = OSME2.hs,
                                    RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.it = OTE.it, OSE.it = OSE.it, 
                                    OME.it = OME.it, ROSE.it = ROSE.it, OSME.it = OSME.it, RME.it = RME.it,
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, OME2.it = OME2.it, ROSE2.it = ROSE2.it, OSME2.it = OSME2.it,
                                    RME2.it = RME2.it, PRICESI.it, PRICESO.it) 
                        } else {
                          res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                    TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.hs = OTE.hs, OSE.hs = OSE.hs, 
                                    OME.hs = OME.hs, ROSE.hs = ROSE.hs, OSME.hs = OSME.hs, RME.hs = RME.hs,
                                    Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                    TFPE2.hs = TFPE2.hs, OME2.hs = OME2.hs, ROSE2.hs = ROSE2.hs, OSME2.hs = OSME2.hs,
                                    RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                    TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.it = OTE.it, OSE.it = OSE.it, 
                                    OME.it = OME.it, ROSE.it = ROSE.it, OSME.it = OSME.it, RME.it = RME.it,
                                    Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                    TFPE2.it = TFPE2.it, OME2.it = OME2.it, ROSE2.it = ROSE2.it, OSME2.it = OSME2.it,
                                    RME2.it = RME2.it, PRICESI.it, PRICESO.it) 
                        }
                      } else {
                        if (orientation == "in") {
                          ITE.hs <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE.hs <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE.hs
                          IME.hs <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts))/ITE.hs
                          RISE.hs <- (AO.hs/(AI.hs * IME.hs * ITE.hs))/MP.hs
                          ISME.hs <- IME.hs * RISE.hs
                          RME.hs <- TFPE.hs/ITE.hs/ISE.hs
                          
                          ITE2.hs <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE2.hs <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2.hs
                          IME2.hs <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts))/ITE2.hs
                          RISE2.hs <- (Qs.hs/(Xs.hs * IME2.hs * ITE2.hs))/MP2.hs
                          ISME2.hs <- IME2.hs * RISE2.hs
                          RME2.hs <- TFPE2.hs/ITE2.hs/ISE2.hs
                          
                          ITE.it <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE.it <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE.it
                          IME.it <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts))/ITE.it
                          RISE.it <- (AO.it/(AI.it * IME.it * ITE.it))/MP.it
                          ISME.it <- IME.it * RISE.it
                          RME.it <- TFPE.it/ITE.it/ISE.it
                          
                          ITE2.it <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE2.it <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2.it
                          IME2.it <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts))/ITE2.it
                          RISE2.it <- (Qs.it/(Xs.it * IME2.it * ITE2.it))/MP2.it
                          ISME2.it <- IME2.it * RISE2.it
                          RME2.it <- TFPE2.it/ITE2.it/ISE2.it
                          
                          if (length(step1) == 6) {
                            REV <- sum(Y.ini[, dmu] * P1[, dmu])
                            COST <- sum(X.ini[, dmu] * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, ITE.hs = ITE.hs, ISE.hs = ISE.hs, 
                                      IME.hs = IME.hs, RISE.hs = RISE.hs, ISME.hs = ISME.hs, RME.hs = RME.hs,
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, IME2.hs = IME2.hs, RISE2.hs = RISE2.hs, ISME2.hs = ISME2.hs,
                                      RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, ITE.it = ITE.it, ISE.it = ISE.it, 
                                      IME.it = IME.it, RISE.it = RISE.it, ISME.it = ISME.it, RME.it = RME.it,
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, IME2.it = IME2.it, RISE2.it = RISE2.it, ISME2.it = ISME2.it,
                                      RME2.it = RME2.it, PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs, 
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, ITE.hs = ITE.hs, ISE.hs = ISE.hs, 
                                      IME.hs = IME.hs, RISE.hs = RISE.hs, ISME.hs = ISME.hs, RME.hs = RME.hs,
                                      Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, TFP2.hs = TFP2.hs, MP2.hs = MP2.hs,
                                      TFPE2.hs = TFPE2.hs, IME2.hs = IME2.hs, RISE2.hs = RISE2.hs, ISME2.hs = ISME2.hs,
                                      RME2.hs = RME2.hs, PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it, 
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, ITE.it = ITE.it, ISE.it = ISE.it, 
                                      IME.it = IME.it, RISE.it = RISE.it, ISME.it = ISME.it, RME.it = RME.it,
                                      Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, TFP2.it = TFP2.it, MP2.it = MP2.it,
                                      TFPE2.it = TFPE2.it, IME2.it = IME2.it, RISE2.it = RISE2.it, ISME2.it = ISME2.it,
                                      RME2.it = RME2.it, PRICESI.it, PRICESO.it) 
                          }
                        } else {
                          OTE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          OSE.hs <- DO.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE.hs
                          OME.hs <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)/OTE.hs
                          ROSE.hs <- ((AO.hs/(OTE.hs * OME.hs))/AI.hs)/MP.hs
                          OSME.hs <- OME.hs * ROSE.hs
                          RME.hs <- TFPE.hs/OTE.hs/OSE.hs
                          # OSME <- OSE * RME
                          
                          OTE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          OSE2.hs <- DO.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs")/OTE2.hs
                          OME2.hs <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.hs, rts)/OTE2.hs
                          ROSE2.hs <- ((Qs.hs/(OTE2.hs * OME2.hs))/Xs.hs)/MP2.hs
                          OSME2.hs <- OME2.hs * ROSE2.hs
                          RME2.hs <- TFPE2.hs/OTE2.hs/OSE2.hs
                          
                          OTE.it <- OTE.hs
                          OSE.it <- OSE.hs
                          OME.it <- DO.ome(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)/OTE.it
                          ROSE.it <- ((AO.it/(OTE.it * OME.it))/AI.it)/MP.it
                          OSME.it <- OME.it * ROSE.it
                          RME.it <- TFPE.it/OTE.it/OSE.it
                          # OSME <- OSE * RME
                          
                          OTE2.it <- OTE2.hs
                          OSE2.it <- OSE2.hs
                          OME2.it <- DO.ome(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESO = PRICESO.it, rts)/OTE2.it
                          ROSE2.it <- ((Qs.it/(OTE2.it * OME2.it))/Xs.it)/MP2.it
                          OSME2.it <- OME2.it * ROSE2.it
                          RME2.it <- TFPE2.it/OTE2.it/OSE2.it
                          
                          ITE.hs <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE.hs <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE.hs
                          IME.hs <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts))/ITE.hs
                          RISE.hs <- (AO.hs/(AI.hs * IME.hs * ITE.hs))/MP.hs
                          ISME.hs <- IME.hs * RISE.hs
                          RME.hs <- TFPE.hs/ITE.hs/ISE.hs
                          
                          ITE2.hs <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE2.hs <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2.hs
                          IME2.hs <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.hs, rts))/ITE2.hs
                          RISE2.hs <- (Qs.hs/(Xs.hs * IME2.hs * ITE2.hs))/MP2.hs
                          ISME2.hs <- IME2.hs * RISE2.hs
                          RME2.hs <- TFPE2.hs/ITE2.hs/ISE2.hs
                          
                          ITE.it <- 1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE.it <- (1/DI.sh(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE.it
                          IME.it <- (1/DI.ime(XOBS = X1[, dmu], YOBS = Y1[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts))/ITE.it
                          RISE.it <- (AO.it/(AI.it * IME.it * ITE.it))/MP.it
                          ISME.it <- IME.it * RISE.it
                          RME.it <- TFPE.it/ITE.it/ISE.it
                          
                          ITE2.it <- 1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts)
                          ISE2.it <- (1/DI.sh(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, rts = "crs"))/ITE2.it
                          IME2.it <- (1/DI.ime(XOBS = X2[, dmu], YOBS = Y2[, dmu], XREF = XREFs, YREF = YREFs, PRICESI = PRICESI.it, rts))/ITE2.it
                          RISE2.it <- (Qs.it/(Xs.it * IME2.it * ITE2.it))/MP2.it
                          ISME2.it <- IME2.it * RISE2.it
                          RME2.it <- TFPE2.it/ITE2.it/ISE2.it
                          
                          OTE.ITE.hs <- sqrt(OTE.hs * ITE.hs)
                          OSE.ISE.hs <- sqrt(OSE.hs * ISE.hs)
                          OME.IME.hs <- sqrt(OME.hs * IME.hs)
                          ROSE.RISE.hs <- sqrt(ROSE.hs * RISE)
                          OSME.ISME.hs <- sqrt(OME.IME.hs * ROSE.RISE.hs)
                          # RME <- TFPE/OTE.ITE/OSE.ISE
                          
                          OTE2.ITE2.hs <- sqrt(OTE2.hs * ITE2.hs)
                          OSE2.ISE2.hs <- sqrt(OSE2.hs * ISE2.hs)
                          OME2.IME2.hs <- sqrt(OME2.hs * IME2.hs)
                          ROSE2.RISE2.hs <- sqrt(ROSE2.hs * RISE2.hs)
                          OSME2.ISME2.hs <- sqrt(OME2.IME2.hs * ROSE2.RISE2.hs)
                          
                          OTE.ITE.it <- sqrt(OTE.it * ITE.it)
                          OSE.ISE.it <- sqrt(OSE.it * ISE.it)
                          OME.IME.it <- sqrt(OME.it * IME.it)
                          ROSE.RISE.it <- sqrt(ROSE.it * RISE)
                          OSME.ISME.it <- sqrt(OME.IME.it * ROSE.RISE.it)
                          # RME <- TFPE/OTE.ITE/OSE.ISE
                          
                          OTE2.ITE2.it <- sqrt(OTE2.it * ITE2.it)
                          OSE2.ISE2.it <- sqrt(OSE2.it * ISE2.it)
                          OME2.IME2.it <- sqrt(OME2.it * IME2.it)
                          ROSE2.RISE2.it <- sqrt(ROSE2.it * RISE2.it)
                          OSME2.ISME2.it <- sqrt(OME2.IME2.it * ROSE2.RISE2.it)
                          
                          if (length(step1) == 6) {
                            REV <- sum(Y.ini[, dmu] * P1[, dmu])
                            COST <- sum(X.ini[, dmu] * W1[, dmu])
                            PROF <- REV/COST
                            P.hs <- REV/AO.hs
                            W.hs <- COST/AI.hs
                            TT.hs <- P.hs/W.hs
                            P.it <- REV/AO.it
                            W.it <- COST/AI.it
                            TT.it <- P.it/W.it
                            res1 <- c(REV = REV, COST = COST, PROF = PROF, P.hs = P.hs, W.hs = W.hs,
                                      TT.hs = TT.hs, AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.ITE.hs = OTE.ITE.hs, OSE.ISE.hs = OSE.ISE.hs,
                                      OME.IME.hs = OME.IME.hs, ROSE.RISE.hs = ROSE.RISE.hs, OSME.ISME.hs = OSME.ISME.hs, 
                                      RME.hs = RME.hs, Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = OME2.IME2.hs,
                                      ROSE2.RISE2.hs = ROSE2.RISE2.hs, OSME2.ISME2.hs = OSME2.ISME2.hs, RME2.hs = RME2.hs, 
                                      PRICESI.hs, PRICESO.hs, P.it = P.it, W.it = W.it, TT.it = TT.it, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.ITE.it = OTE.ITE.it, OSE.ISE.it = OSE.ISE.it,
                                      OME.IME.it = OME.IME.it, ROSE.RISE.it = ROSE.RISE.it, OSME.ISME.it = OSME.ISME.it, 
                                      RME.it = RME.it, Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = OME2.IME2.it,
                                      ROSE2.RISE2.it = ROSE2.RISE2.it, OSME2.ISME2.it = OSME2.ISME2.it, RME2.it = RME2.it, 
                                      PRICESI.it, PRICESO.it)
                          } else {
                            res1 <- c(AO.hs = AO.hs, AI.hs = AI.hs,
                                      TFP.hs = TFP.hs, MP.hs = MP.hs, TFPE.hs = TFPE.hs, OTE.ITE.hs = OTE.ITE.hs, OSE.ISE.hs = OSE.ISE.hs,
                                      OME.IME.hs = OME.IME.hs, ROSE.RISE.hs = ROSE.RISE.hs, OSME.ISME.hs = OSME.ISME.hs, 
                                      RME.hs = RME.hs, Qt.hs = Qt.hs, Qs.hs = Qs.hs, Xt.hs = Xt.hs, Xs.hs = Xs.hs, 
                                      TFP2.hs = TFP2.hs, MP2.hs = MP2.hs, TFPE2.hs = TFPE2.hs, OME2.IME2.hs = OME2.IME2.hs,
                                      ROSE2.RISE2.hs = ROSE2.RISE2.hs, OSME2.ISME2.hs = OSME2.ISME2.hs, RME2.hs = RME2.hs, 
                                      PRICESI.hs, PRICESO.hs, AO.it = AO.it, AI.it = AI.it,
                                      TFP.it = TFP.it, MP.it = MP.it, TFPE.it = TFPE.it, OTE.ITE.it = OTE.ITE.it, OSE.ISE.it = OSE.ISE.it,
                                      OME.IME.it = OME.IME.it, ROSE.RISE.it = ROSE.RISE.it, OSME.ISME.it = OSME.ISME.it, 
                                      RME.it = RME.it, Qt.it = Qt.it, Qs.it = Qs.it, Xt.it = Xt.it, Xs.it = Xs.it, 
                                      TFP2.it = TFP2.it, MP2.it = MP2.it, TFPE2.it = TFPE2.it, OME2.IME2.it = OME2.IME2.it,
                                      ROSE2.RISE2.it = ROSE2.RISE2.it, OSME2.ISME2.it = OSME2.ISME2.it, RME2.it = RME2.it, 
                                      PRICESI.it, PRICESO.it)
                          }
                        }
                      }
                      return(res1)
                    }
  }
res2
}

### Hicks-Moorsteen, print fonction
print.HicksMoorsteen <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- max(3, getOption("digits") - 3)
  }
  if (length(x) == 2) {
    cat("\nHicks-Moorsteen productivity and profitability levels (summary):\n\n")
    print(summary(x[["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\nHicks-Moorsteen productivity and profitability changes (summary):\n\n")
    print(summary(x[["Changes"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n")
    invisible(x)
  } else {
    cat("\n## Hicks-Moorsteen ##\n")
    cat("\n- Hicks-Moorsteen productivity and profitability levels (summary):\n\n")
    print(summary(x[["HicksMoorsteen"]][["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\n- Hicks-Moorsteen productivity and profitability changes (summary):\n\n")
    print(summary(x[["HicksMoorsteen"]][["Changes"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n==============\n")
    cat("\n## Malmquist-hs ##\n")
    cat("\n- Malmquist-hs productivity and profitability levels (summary):\n\n")
    print(summary(x[["MalmquistHS"]][["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\n- Malmquist-hs productivity and profitability changes (summary):\n\n")
    print(summary(x[["MalmquistHS"]][["Changes"]][-c(1:2)], digits = digits), digits = digits)
      if (!is.null(x[["MalmquistHS"]][["Shadowp"]])) {
        cat("\n\n- Malmquist-hs productivity shadow prices (summary):\n\n")
        print(summary(x[["MalmquistHS"]][["Shadowp"]][-c(1:2)], digits = digits), digits = digits)
      }
    cat("\n==============\n")
    cat("\n## Malmquist-it ##\n")
    cat("\n- Malmquist-it productivity and profitability levels (summary):\n\n")
    print(summary(x[["MalmquistIT"]][["Levels"]][-c(1:2)], digits = digits), digits = digits)
    cat("\n\n- Malmquist-it productivity and profitability changes (summary):\n\n")
    print(summary(x[["MalmquistIT"]][["Changes"]][-c(1:2)], digits = digits), digits = digits)
      if (!is.null(x[["MalmquistIT"]][["Shadowp"]])) {
        cat("\n\n- Malmquist-it productivity shadow prices (summary):\n\n")
        print(summary(x[["MalmquistIT"]][["Shadowp"]][-c(1:2)], digits = digits), digits = digits)
      }
    cat("\n")
    invisible(x)
  }
}
