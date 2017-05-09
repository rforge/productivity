## Productivity package auxiliary functions

# Data check
check.1 <- function(data, id.var, time.var, x.vars, y.vars, w.vars, p.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables. Only one can be defined", call. = FALSE)
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
    stop("Missing time variable", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables. Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var :", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variable", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars :", paste(x.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variable", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars :", paste(y.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
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
        stop("Unrecognizable variables in w.vars :", paste(w.vars[var_logical == 
          F], collapse = ","), call. = FALSE)
      if (is.numeric(p.vars)) {
        p.vars.1 <- names.var[p.vars]
      } else {
        p.vars.1 <- p.vars
      }
      var_logical <- p.vars.1 %in% names.var
      if (!(all(var_logical))) 
        stop("Unrecognizable variables in p.vars :", paste(p.vars[var_logical == 
          F], collapse = ","), call. = FALSE)
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
    stop("Missing id variable", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables. Only one can be defined", call. = FALSE)
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
    stop("Missing time variable", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables. Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var :", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variable", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars :", paste(x.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variable", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars :", paste(y.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
  }
  if (missing(w.vars)) {
    stop("Missing w variable", call. = FALSE)
  } else {
    if (is.numeric(w.vars)) {
      w.vars.1 <- names.var[w.vars]
    } else {
      w.vars.1 <- w.vars
    }
    var_logical <- w.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in w.vars :", paste(w.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
  }
  if (missing(p.vars)) {
    stop("Missing p variable", call. = FALSE)
  } else {
    if (is.numeric(p.vars)) {
      p.vars.1 <- names.var[p.vars]
    } else {
      p.vars.1 <- p.vars
    }
    var_logical <- p.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in p.vars :", paste(p.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
  }
  if (length(w.vars) != length(x.vars)) 
    stop("x.vars and w.vars must be of the same length", call. = FALSE)
  if (length(p.vars) != length(y.vars)) 
    stop("y.vars and p.vars must be of the same length", call. = FALSE)
  list(id.var = id.var.1, time.var = time.var.1, x.vars = x.vars.1, y.vars = y.vars.1, 
    w.vars = w.vars.1, p.vars = p.vars.1)
}

check.3 <- function(data, id.var, time.var, x.vars, y.vars) {
  if (!(is.data.frame(data))) 
    stop("data must be a dataframe")
  names.var <- names(data)
  if (missing(id.var)) {
    stop("Missing id variable", call. = FALSE)
  } else {
    if (length(id.var) > 1) 
      stop("Too many id variables. Only one can be defined", call. = FALSE)
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
    stop("Missing time variable", call. = FALSE)
  } else {
    if (length(time.var) > 1) 
      stop("Too many time variables. Only one can be defined", call. = FALSE)
    if (is.numeric(time.var)) {
      time.var.1 <- names.var[time.var]
    } else {
      time.var.1 <- time.var
    }
    var_logical <- time.var.1 %in% names.var
    if (!var_logical) 
      stop("Unrecognizable variable in time.var :", paste(time.var), call. = FALSE)
  }
  if (missing(x.vars)) {
    stop("Missing x variable", call. = FALSE)
  } else {
    if (is.numeric(x.vars)) {
      x.vars.1 <- names.var[x.vars]
    } else {
      x.vars.1 <- x.vars
    }
    var_logical <- x.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in x.vars :", paste(x.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
  }
  if (missing(y.vars)) {
    stop("Missing y variable", call. = FALSE)
  } else {
    if (is.numeric(y.vars)) {
      y.vars.1 <- names.var[y.vars]
    } else {
      y.vars.1 <- y.vars
    }
    var_logical <- y.vars.1 %in% names.var
    if (!(all(var_logical))) 
      stop("Unrecognizable variables in y.vars :", paste(y.vars[var_logical == 
        F], collapse = ","), call. = FALSE)
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
    cbind(matrix(0, nrow = n_x, ncol = n_y), diag(-1, nrow = n_x, ncol = n_x), 
      XREF, matrix(0, nrow = n_x, ncol = 1))
  }
  mat_y <- if (n_y == 1) {
    c(-1, rep(0, n_x), YREF, 0)
  } else {
    cbind(diag(-1, ncol = n_y, nrow = n_y), matrix(0, nrow = n_y, ncol = n_x), 
      YREF, matrix(0, nrow = n_y, ncol = 1))
  }
  if (rts == "vrs") {
    mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)), c(rep(0, 
      n_x + n_y), rep(1, n_t), -1))
    dir <- c(rep("<=", n_x), rep(">=", n_y), "==", "==")
    rhs <- c(rep(0, n_x), rep(0, n_y), 1, 0)
  } else {
    if (rts == "nirs") {
      mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)), 
        c(rep(0, n_x + n_y), rep(1, n_t), -1))
      dir <- c(rep("<=", n_x), rep(">=", n_y), "==", "<=")
      rhs <- c(rep(0, n_x), rep(0, n_y), 1, 0)
    } else {
      if (rts == "ndrs") {
        mat <- rbind(mat_x, mat_y, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)), 
          c(rep(0, n_x + n_y), rep(1, n_t), -1))
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
