## Productivity package auxiliary functions

# Data check
check.1 <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables (id.var). Only one can be defined", call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in id.var: ", paste(id.var), call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables (time.var). Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var:", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (is.null(w.vars) & is.null(p.vars)) {
    list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1)
  } else {
    if (!(is.null(w.vars)) & !(is.null(p.vars))) {
      if (length(w.vars) != length(x.vars)) 
        stop("x.vars and w.vars must be of the same length", call. = FALSE)
      if (length(p.vars) != length(y.vars)) 
        stop("y.vars and p.vars must be of the same length", call. = FALSE)
      if (is.numeric(w.vars)) {
        w.vars.1 <- names.var[w.vars]
      } else {
        w.vars.1 <- w.vars
      }
      var_logical <- w.vars.1 %in% names.var
      if (!(all(var_logical))) 
        stop("Unrecognizable variables in w.vars:", paste(w.vars[var_logical == F], 
          collapse = ","), call. = FALSE)
      if (is.numeric(p.vars)) {
        p.vars.1 <- names.var[p.vars]
      } else {
        p.vars.1 <- p.vars
      }
      var_logical <- p.vars.1 %in% names.var
      if (!(all(var_logical))) 
        stop("Unrecognizable variables in p.vars:", paste(p.vars[var_logical == F], 
          collapse = ","), call. = FALSE)
      list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1, 
        w.vars = w.vars.1, p.vars = p.vars.1)
    } else {
      if (!(is.null(w.vars)) & is.null(p.vars)) {
        stop("Output prices p.vars must also be supplied", call. = FALSE)
      } else {
        stop("Input prices w.vars must also be supplied", call. = FALSE)
      }
    }
  }
}

check.2 <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables (id.var). Only one can be defined", call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in id.var: ", paste(id.var), call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables (time.var). Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var:", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(w.vars)) {
    stop("Missing w variables (w.vars)", call. = FALSE)
  } else {
    if (is.numeric(w.vars)) {
      w.vars.1 <- names.var[w.vars]
    } else {
      w.vars.1 <- w.vars
    }
    var_logical <- w.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in w.vars:", paste(w.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(p.vars)) {
    stop("Missing p variables (p.vars)", call. = FALSE)
  } else {
    if (is.numeric(p.vars)) {
      p.vars.1 <- names.var[p.vars]
    } else {
      p.vars.1 <- p.vars
    }
    var_logical <- p.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in p.vars:", paste(p.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (length(w.vars) != length(x.vars)) 
    stop("x.vars and w.vars must be of the same length", call. = FALSE)
  if (length(p.vars) != length(y.vars)) 
    stop("y.vars and p.vars must be of the same length", call. = FALSE)
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1, w.vars = w.vars.1, 
    p.vars = p.vars.1)
}

check.3 <- function(data, id.var, time.var, x.vars, y.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable (id.var)", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables (id.var). Only one can be defined", call. = FALSE)
    if (is.numeric(id.var)) {
      id.var.1 <- names.var[id.var]
    } else {
      id.var.1 <- id.var
    }
    var_logical <- id.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in id.var: ", paste(id.var), call. = FALSE)
  }
  if (missing(time.var)) {
    stop("Missing time variable (time.var)", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables (time.var). Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var:", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variables (x.vars)", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars:", paste(x.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variables (y.vars)", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars:", paste(y.vars[var_logical == F], collapse = ","), 
        call. = FALSE)
  }
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1)
}

# Shephard distance function
DO.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  obj <- c(1, rep(0, n_t))
  mat_x <- if (n_x == 1) {
    c(0, XREF)
  } else {
    cbind(matrix(0, nrow = n_x, ncol = 1), XREF)
  }
  mat_y <- if (n_y == 1) {
    c(-YOBS, YREF)
  } else {
    cbind(-matrix(YOBS, ncol = 1), YREF)
  }
  if (rts == "vrs") {
    # variable returns to scale
    mat <- rbind(mat_x, mat_y, c(0, rep(1, n_t)))
    dir <- c(rep("<=", n_x), rep(">=", n_y), "==")
    rhs <- c(XOBS, rep(0, n_y), 1)
  } else {
    if (rts == "nirs") {
      # nonincreasing returns to scale
      mat <- rbind(mat_x, mat_y, c(0, rep(1, n_t)))
      dir <- c(rep("<=", n_x), rep(">=", n_y), "<=")
      rhs <- c(XOBS, rep(0, n_y), 1)
    } else {
      if (rts == "ndrs") {
        # nondecreasing returns to scale
        mat <- rbind(mat_x, mat_y, c(0, rep(1, n_t)))
        dir <- c(rep("<=", n_x), rep(">=", n_y), ">=")
        rhs <- c(XOBS, rep(0, n_y), 1)
      } else {
        # constant returns to scale
        mat <- rbind(mat_x, mat_y)
        dir <- c(rep("<=", n_x), rep(">=", n_y))
        rhs <- c(XOBS, rep(0, n_y))
      }
    }
  }
  max <- TRUE
  bounds <- list(lower = list(ind = 1L, val = -Inf), upper = list(ind = 1L, val = Inf))
  opt <- Rglpk_solve_LP(obj, mat, dir, rhs, max = max, bounds = bounds)
  DO <- 1/opt$optimum
  DO
}

DI.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  obj <- c(1, rep(0, n_t))
  mat_x <- if (n_x == 1) {
    c(-XOBS, XREF)
  } else {
    cbind(-matrix(XOBS, nrow = n_x), XREF)
  }
  mat_y <- if (n_y == 1) {
    c(0, YREF)
  } else {
    cbind(matrix(0, nrow = n_y, ncol = 1), YREF)
  }
  if (rts == "vrs") {
    mat <- rbind(mat_x, mat_y, c(0, rep(1, n_t)))
    dir <- c(rep("<=", n_x), rep(">=", n_y), "==")
    rhs <- c(rep(0, n_x), YOBS, 1)
  } else {
    if (rts == "nirs") {
      mat <- rbind(mat_x, mat_y, c(0, rep(1, n_t)))
      dir <- c(rep("<=", n_x), rep(">=", n_y), "<=")
      rhs <- c(rep(0, n_x), YOBS, 1)
    } else {
      if (rts == "ndrs") {
        mat <- rbind(mat_x, mat_y, c(0, rep(1, n_t)))
        dir <- c(rep("<=", n_x), rep(">=", n_y), ">=")
        rhs <- c(rep(0, n_x), YOBS, 1)
      } else {
        mat <- rbind(mat_x, mat_y)
        dir <- c(rep("<=", n_x), rep(">=", n_y))
        rhs <- c(rep(0, n_x), YOBS)
      }
    }
  }
  max <- FALSE
  bounds <- list(lower = list(ind = 1L, val = -Inf), upper = list(ind = 1L, val = Inf))
  opt <- Rglpk_solve_LP(obj, mat, dir, rhs, max = max, bounds = bounds)
  DI <- 1/opt$optimum
  DI
}

# Other TFP components functions
DO.ome <- function(XOBS, YOBS, XREF, YREF, PRICESO, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  obj <- c(PRICESO/sum(YOBS * PRICESO), rep(0, n_t))
  mat_x <- if (n_x == 1) {
    c(rep(0, n_y), XREF)
  } else {
    cbind(matrix(0, nrow = n_x, ncol = n_y), XREF)
  }
  mat_y <- if (n_y == 1) {
    c(-1, YREF)
  } else {
    cbind(-diag(1, ncol = n_y, nrow = n_y), YREF)
  }
  if (rts == "vrs") {
    mat <- rbind(mat_x, mat_y, c(rep(0, n_y), rep(1, n_t)))
    dir <- c(rep("<=", n_x), rep(">=", n_y), "==")
    rhs <- c(XOBS, rep(0, n_y), 1)
  } else {
    if (rts == "nirs") {
      mat <- rbind(mat_x, mat_y, c(rep(0, n_y), rep(1, n_t)))
      dir <- c(rep("<=", n_x), rep(">=", n_y), "<=")
      rhs <- c(XOBS, rep(0, n_y), 1)
    } else {
      if (rts == "ndrs") {
        mat <- rbind(mat_x, mat_y, c(rep(0, n_y), rep(1, n_t)))
        dir <- c(rep("<=", n_x), rep(">=", n_y), ">=")
        rhs <- c(XOBS, rep(0, n_y), 1)
      } else {
        mat <- rbind(mat_x, mat_y)
        dir <- c(rep("<=", n_x), rep(">=", n_y))
        rhs <- c(XOBS, rep(0, n_y))
      }
    }
  }
  max <- TRUE
  opt <- Rglpk_solve_LP(obj, mat, dir, rhs, max = max)
  DO.ome <- 1/opt$optimum
  DO.ome
}

DI.ime <- function(XOBS, YOBS, XREF, YREF, PRICESI, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  obj <- c(PRICESI/sum(XOBS * PRICESI), rep(0, n_t))
  mat_x <- if (n_x == 1) {
    c(-1, XREF)
  } else {
    cbind(-diag(1, nrow = n_x, ncol = n_x), XREF)
  }
  mat_y <- if (n_y == 1) {
    c(rep(0, n_x), YREF)
  } else {
    cbind(matrix(0, nrow = n_y, ncol = n_x), YREF)
  }
  if (rts == "vrs") {
    mat <- rbind(mat_x, mat_y, c(rep(0, n_x), rep(1, n_t)))
    dir <- c(rep("<=", n_x), rep(">=", n_y), "==")
    rhs <- c(rep(0, n_x), YOBS, 1)
  } else {
    if (rts == "nirs") {
      mat <- rbind(mat_x, mat_y, c(rep(0, n_x), rep(1, n_t)))
      dir <- c(rep("<=", n_x), rep(">=", n_y), "<=")
      rhs <- c(rep(0, n_x), YOBS, 1)
    } else {
      if (rts == "ndrs") {
        mat <- rbind(mat_x, mat_y, c(rep(0, n_x), rep(1, n_t)))
        dir <- c(rep("<=", n_x), rep(">=", n_y), ">=")
        rhs <- c(rep(0, n_x), YOBS, 1)
      } else {
        mat <- rbind(mat_x, mat_y)
        dir <- c(rep("<=", n_x), rep(">=", n_y))
        rhs <- c(rep(0, n_x), YOBS)
      }
    }
  }
  max <- FALSE
  opt <- Rglpk_solve_LP(obj, mat, dir, rhs, max = max)
  DI.ime <- 1/opt$optimum
  DI.ime
}

D.tfp <- function(XOBS, YOBS, XREF, YREF, PRICESO, PRICESI, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  obj <- c(PRICESO, rep(0, n_x), rep(0, n_t), 0)
  mat_x <- if (n_x == 1) {
    c(rep(0, n_y), -1, XREF, 0)
  } else {
    cbind(matrix(0, nrow = n_x, ncol = n_y), diag(-1, nrow = n_x, ncol = n_x), XREF, matrix(0, 
      nrow = n_x, ncol = 1))
  }
  mat_y <- if (n_y == 1) {
    c(-1, rep(0, n_x), YREF, 0)
  } else {
    cbind(diag(-1, ncol = n_y, nrow = n_y), matrix(0, nrow = n_y, ncol = n_x), YREF, matrix(0, 
      nrow = n_y, ncol = 1))
  }
  if (rts == "vrs") {
    mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)), c(rep(0, n_x + 
      n_y), rep(1, n_t), -1))
    dir <- c(rep("<=", n_x), rep(">=", n_y), "==", "==")
    rhs <- c(rep(0, n_x), rep(0, n_y), 1, 0)
  } else {
    if (rts == "nirs") {
      mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)), c(rep(0, n_x + 
        n_y), rep(1, n_t), -1))
      dir <- c(rep("<=", n_x), rep(">=", n_y), "==", "<=")
      rhs <- c(rep(0, n_x), rep(0, n_y), 1, 0)
    } else {
      if (rts == "ndrs") {
        mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)), c(rep(0, 
          n_x + n_y), rep(1, n_t), -1))
        dir <- c(rep("<=", n_x), rep(">=", n_y), "==", ">=")
        rhs <- c(rep(0, n_x), rep(0, n_y), 1, 0)
      } else {
        mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)))
        dir <- c(rep("<=", n_x), rep(">=", n_y), "==")
        rhs <- c(rep(0, n_x), rep(0, n_y), 1)
      }
    }
  }
  max <- TRUE
  opt <- Rglpk_solve_LP(obj, mat, dir, rhs, max = max)
  tfp <- opt$optimum
  tfp
}  #XOBS and YOBS are not actually useful for this computation. We keep it for the form

## dual shephard output distance function

DO.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  if (rts == "vrs") {
    objo <- c(rep(0, n_y), XOBS, 1)
    mato <- rbind(cbind(-t(YREF), t(XREF), matrix(1, nrow = n_t, ncol = 1)), c(YOBS, rep(0, 
      n_x), 0))
    diro <- c(rep(">=", n_t), "==")
    rhso <- c(rep(0, n_t), 1)
    maxo <- FALSE
    boundso <- list(lower = list(ind = (n_y + n_x + 1), val = -Inf), upper = list(ind = (n_y + 
      n_x + 1), val = Inf))
    opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo, 
      bounds = boundso)
    prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * XOBS) + 
      opto$solution[n_y + n_x + 1])
  } else {
    if (rts == "nirs") {
      objo <- c(rep(0, n_y), XOBS, 1)
      mato <- rbind(cbind(-t(YREF), t(XREF), matrix(1, nrow = n_t, ncol = 1)), c(YOBS, 
        rep(0, n_x), 0))
      diro <- c(rep(">=", n_t), "==")
      rhso <- c(rep(0, n_t), 1)
      maxo <- FALSE
      opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo)
      prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * XOBS) + 
        opto$solution[n_y + n_x + 1])
    } else {
      if (rts == "ndrs") {
        objo <- c(rep(0, n_y), XOBS, -1)
        mato <- rbind(cbind(-t(YREF), t(XREF), matrix(-1, nrow = n_t, ncol = 1)), c(YOBS, 
          rep(0, n_x), 0))
        diro <- c(rep(">=", n_t), "==")
        rhso <- c(rep(0, n_t), 1)
        maxo <- FALSE
        opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo)
        prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * 
          XOBS) - opto$solution[n_y + n_x + 1])
      } else {
        objo <- c(rep(0, n_y), XOBS)
        mato <- rbind(cbind(-t(YREF), t(XREF)), c(YOBS, rep(0, n_x)))
        diro <- c(rep(">=", n_t), "==")
        rhso <- c(rep(0, n_t), 1)
        maxo <- FALSE
        opto <- Rglpk_solve_LP(obj = objo, mat = mato, dir = diro, rhs = rhso, max = maxo)
        prices_o <- opto$solution[1:n_y]/(sum(opto$solution[(1 + n_y):(n_y + n_x)] * 
          XOBS))
      }
    }
  }
  names(prices_o) <- paste("U", 1:n_y, sep = "")
  prices_o
}

## dual shephard input distance function

DI.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  if (rts == "vrs") {
    obji <- c(YOBS, rep(0, n_x), 1)
    mati <- rbind(cbind(t(YREF), -t(XREF), matrix(1, nrow = n_t, ncol = 1)), c(rep(0, n_y), 
      XOBS, 0))
    diri <- c(rep("<=", n_t), "==")
    rhsi <- c(rep(0, n_t), 1)
    maxi <- TRUE
    boundsi <- list(lower = list(ind = (n_y + n_x + 1), val = -Inf), upper = list(ind = (n_y + 
      n_x + 1), val = Inf))
    opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi, 
      bounds = boundsi)
    prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * YOBS) + 
      opti$solution[n_x + n_y + 1])
  } else {
    if (rts == "nirs") {
      obji <- c(YOBS, rep(0, n_x), -1)
      mati <- rbind(cbind(t(YREF), -t(XREF), matrix(-1, nrow = n_t, ncol = 1)), c(rep(0, 
        n_y), XOBS, 0))
      diri <- c(rep("<=", n_t), "==")
      rhsi <- c(rep(0, n_t), 1)
      maxi <- TRUE
      opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi)
      prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * YOBS) - 
        opti$solution[n_x + n_y + 1])
    } else {
      if (rts == "ndrs") {
        obji <- c(YOBS, rep(0, n_x), 1)
        mati <- rbind(cbind(t(YREF), -t(XREF), matrix(1, nrow = n_t, ncol = 1)), c(rep(0, 
          n_y), XOBS, 0))
        diri <- c(rep("<=", n_t), "==")
        rhsi <- c(rep(0, n_t), 1)
        maxi <- TRUE
        opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi)
        prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * 
          YOBS) + opti$solution[n_x + n_y + 1])
      } else {
        obji <- c(YOBS, rep(0, n_x))
        mati <- rbind(cbind(t(YREF), -t(XREF)), c(rep(0, n_y), XOBS))
        diri <- c(rep("<=", n_t), "==")
        rhsi <- c(rep(0, n_t), 1)
        maxi <- TRUE
        opti <- Rglpk_solve_LP(obj = obji, mat = mati, dir = diri, rhs = rhsi, max = maxi)
        prices_i <- opti$solution[(n_y + 1):(n_y + n_x)]/(sum(opti$solution[1:n_y] * 
          YOBS))
      }
    }
  }
  names(prices_i) <- paste("V", 1:n_x, sep = "")
  prices_i
}

## apply a function to divide 2 columns
fdiv <- function(x) x[, 1]/x[, 2]

## Return functions (i.e. Levels(); Changes(); Shadowp())
Levels <- function(object, ...) {
    if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche"))) {
        stop("Function 'Levels' can not be applied to an object of class \"", class(object), "\"")
    }
    return(object$Levels)
}

Changes <- function(object, ...) {
    if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche"))) {
        stop("Function 'Changes' can not be applied to an object of class \"", class(object), "\"")
    }
    return(object$Changes)
}

Shadowp <- function(object, ...) {
    if (is(object, c("Malmquist"))) {
        stop("Function 'Shadowp' can not be applied to an object of class \"", class(object)[2], "\"")
    }
    if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Paasche"))) {
        stop("Function 'Shadowp' can not be applied to an object of class \"", class(object), "\"")
    }
    return(object$Shadowp)
}