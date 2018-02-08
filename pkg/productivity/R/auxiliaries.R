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
  
  if (rts == "vrs") {
    # variable returns to scale
    built.lp <- make.lp(n_x + n_y + 1, 1 + n_t)
    for (i in 1:n_x) {
      set.row(built.lp, i, c(0, XREF[i, ]))
    }
    for (j in 1:n_y) {
      set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
    }
    set.row(built.lp, n_x + n_y + 1, c(0, rep(1, n_t)))
    set.objfn(built.lp, c(1, rep(0, n_t)))
    set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "="))
    set.rhs (built.lp, c(XOBS, rep(0, n_y), 1))
    set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  } else {
    if (rts == "nirs") {
      # nonincreasing returns to scale
      built.lp <- make.lp(n_x + n_y + 1, 1 + n_t)
      for (i in 1:n_x) {
        set.row(built.lp, i, c(0, XREF[i, ]))
      }
      for (j in 1:n_y) {
        set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
      }
      set.row(built.lp, n_x + n_y + 1, c(0, rep(1, n_t)))
      set.objfn(built.lp, c(1, rep(0, n_t)))
      set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "<="))
      set.rhs (built.lp, c(XOBS, rep(0, n_y), 1))
      set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
    } else {
      if (rts == "ndrs") {
        # nondecreasing returns to scale
        built.lp <- make.lp(n_x + n_y + 1, 1 + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(0, XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
        }
        set.row(built.lp, n_x + n_y + 1, c(0, rep(1, n_t)))
        set.objfn(built.lp, c(1, rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), ">="))
        set.rhs (built.lp, c(XOBS, rep(0, n_y), 1))
        set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
      } else {
        # constant returns to scale
        built.lp <- make.lp(n_x + n_y, 1 + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(0, XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(-YOBS[j], YREF[j, ]))
        }
        set.objfn(built.lp, c(1, rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
        set.rhs (built.lp, c(XOBS, rep(0, n_y)))
        set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
      }
    }
  }
  
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  DO <- 1/get.objective(built.lp)
  DO
}

DI.sh <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  
  if (rts == "vrs") {
    built.lp <- make.lp(n_x + n_y + 1, 1 + n_t)
    for (i in 1:n_x) {
      set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
    }
    for (j in 1:n_y) {
      set.row(built.lp, n_x + j, c(0, YREF[j, ]))
    }
    set.row(built.lp, n_x + n_y + 1, c(0, rep(1, n_t)))
    set.objfn(built.lp, c(1, rep(0, n_t)))
    set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "="))
    set.rhs (built.lp, c(rep(0, n_x), YOBS, 1))
    set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
  } else {
    if (rts == "nirs") {
      built.lp <- make.lp(n_x + n_y + 1, 1 + n_t)
      for (i in 1:n_x) {
        set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
      }
      for (j in 1:n_y) {
        set.row(built.lp, n_x + j, c(0, YREF[j, ]))
      }
      set.row(built.lp, n_x + n_y + 1, c(0, rep(1, n_t)))
      set.objfn(built.lp, c(1, rep(0, n_t)))
      set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "<="))
      set.rhs (built.lp, c(rep(0, n_x), YOBS, 1))
      set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
    } else {
      if (rts == "ndrs") {
        built.lp <- make.lp(n_x + n_y + 1, 1 + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(0, YREF[j, ]))
        }
        set.row(built.lp, n_x + n_y + 1, c(0, rep(1, n_t)))
        set.objfn(built.lp, c(1, rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), ">="))
        set.rhs (built.lp, c(rep(0, n_x), YOBS, 1))
        set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
      } else {
        built.lp <- make.lp(n_x + n_y, 1 + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(-XOBS[i], XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(0, YREF[j, ]))
        }
        set.objfn(built.lp, c(1, rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
        set.rhs (built.lp, c(rep(0, n_x), YOBS))
        set.bounds(built.lp, lower = -Inf, upper = Inf, columns = 1)
      }
    }
  }
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  DI <- 1/get.objective(built.lp)
  DI
}

# Other TFP components functions
DO.ome <- function(XOBS, YOBS, XREF, YREF, PRICESO, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  
  if (rts == "vrs") {
    built.lp <- make.lp(n_x + n_y + 1, n_y + n_t)
    for (i in 1:n_x) {
      set.row(built.lp, i, c(rep(0, n_y), XREF[i, ]))
    }
    for (j in 1:n_y) {
      set.row(built.lp, n_x + j, c(-diag(1, ncol = n_y, nrow = n_y)[j, ], YREF[j, ]))
    }
    set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), rep(1, n_t)))
    set.objfn(built.lp, c(PRICESO/sum(YOBS * PRICESO), rep(0, n_t)))
    set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "="))
    set.rhs (built.lp, c(XOBS, rep(0, n_y), 1))
  } else {
    if (rts == "nirs") {
      built.lp <- make.lp(n_x + n_y + 1, n_y + n_t)
      for (i in 1:n_x) {
        set.row(built.lp, i, c(rep(0, n_y), XREF[i, ]))
      }
      for (j in 1:n_y) {
        set.row(built.lp, n_x + j, c(-diag(1, ncol = n_y, nrow = n_y)[j, ], YREF[j, ]))
      }
      set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), rep(1, n_t)))
      set.objfn(built.lp, c(PRICESO/sum(YOBS * PRICESO), rep(0, n_t)))
      set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "<="))
      set.rhs (built.lp, c(XOBS, rep(0, n_y), 1))
    } else {
      if (rts == "ndrs") {
        built.lp <- make.lp(n_x + n_y + 1, n_y + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(rep(0, n_y), XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(-diag(1, ncol = n_y, nrow = n_y)[j, ], YREF[j, ]))
        }
        set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), rep(1, n_t)))
        set.objfn(built.lp, c(PRICESO/sum(YOBS * PRICESO), rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), ">="))
        set.rhs (built.lp, c(XOBS, rep(0, n_y), 1))
      } else {
        built.lp <- make.lp(n_x + n_y, n_y + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(rep(0, n_y), XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(-diag(1, ncol = n_y, nrow = n_y)[j, ], YREF[j, ]))
        }
        set.objfn(built.lp, c(PRICESO/sum(YOBS * PRICESO), rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
        set.rhs (built.lp, c(XOBS, rep(0, n_y)))
      }
    }
  }
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  DO.ome <- 1/get.objective(built.lp)
  DO.ome
}

DI.ime <- function(XOBS, YOBS, XREF, YREF, PRICESI, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]

  if (rts == "vrs") {
    built.lp <- make.lp(n_x + n_y + 1, n_x + n_t)
    for (i in 1:n_x) {
      set.row(built.lp, i, c(-diag(1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ]))
    }
    for (j in 1:n_y) {
      set.row(built.lp, n_x + j, c(rep(0, n_x), YREF[j, ]))
    }
    set.row(built.lp, n_x + n_y + 1, c(rep(0, n_x), rep(1, n_t)))
    set.objfn(built.lp, c(PRICESI/sum(XOBS * PRICESI), rep(0, n_t)))
    set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "="))
    set.rhs (built.lp, c(rep(0, n_x), YOBS, 1))
  } else {
    if (rts == "nirs") {
      built.lp <- make.lp(n_x + n_y + 1, n_x + n_t)
      for (i in 1:n_x) {
        set.row(built.lp, i, c(-diag(1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ]))
      }
      for (j in 1:n_y) {
        set.row(built.lp, n_x + j, c(rep(0, n_x), YREF[j, ]))
      }
      set.row(built.lp, n_x + n_y + 1, c(rep(0, n_x), rep(1, n_t)))
      set.objfn(built.lp, c(PRICESI/sum(XOBS * PRICESI), rep(0, n_t)))
      set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "<="))
      set.rhs (built.lp, c(rep(0, n_x), YOBS, 1))
    } else {
      if (rts == "ndrs") {
        built.lp <- make.lp(n_x + n_y + 1, n_x + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(-diag(1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(rep(0, n_x), YREF[j, ]))
        }
        set.row(built.lp, n_x + n_y + 1, c(rep(0, n_x), rep(1, n_t)))
        set.objfn(built.lp, c(PRICESI/sum(XOBS * PRICESI), rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), ">="))
        set.rhs (built.lp, c(rep(0, n_x), YOBS, 1))
      } else {
        built.lp <- make.lp(n_x + n_y , n_x + n_t)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(-diag(1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ]))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(rep(0, n_x), YREF[j, ]))
        }
        set.objfn(built.lp, c(PRICESI/sum(XOBS * PRICESI), rep(0, n_t)))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y)))
        set.rhs (built.lp, c(rep(0, n_x), YOBS))
      }
    }
  }
  lp.control(built.lp, sense = "min")
  solve(built.lp)
  DI.ime <- 1/get.objective(built.lp)
  DI.ime
}

D.tfp <- function(XOBS, YOBS, XREF, YREF, PRICESO, PRICESI, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  
  if (rts == "vrs") {
    built.lp <- make.lp(n_x + n_y + 2, n_y + n_x + n_t + 1)
    for (i in 1:n_x) {
      set.row(built.lp, i, c(rep(0, n_y), diag(-1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ], 0))
    }
    for (j in 1:n_y) {
      set.row(built.lp, n_x + j, c(diag(-1, ncol = n_y, nrow = n_y)[j, ], rep(0, n_x), YREF[j, ], 0))
    }
    set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)))
    set.row(built.lp, n_x + n_y + 2, c(rep(0, n_x + n_y), rep(1, n_t), -1))
    set.objfn(built.lp, c(PRICESO, rep(0, n_x), rep(0, n_t), 0))
    set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "=", "="))
    set.rhs (built.lp, c(rep(0, n_x), rep(0, n_y), 1, 0))
  } else {
    if (rts == "nirs") {
      built.lp <- make.lp(n_x + n_y + 2, n_y + n_x + n_t + 1)
      for (i in 1:n_x) {
        set.row(built.lp, i, c(rep(0, n_y), diag(-1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ], 0))
      }
      for (j in 1:n_y) {
        set.row(built.lp, n_x + j, c(diag(-1, ncol = n_y, nrow = n_y)[j, ], rep(0, n_x), YREF[j, ], 0))
      }
      set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)))
      set.row(built.lp, n_x + n_y + 2, c(rep(0, n_x + n_y), rep(1, n_t), -1))
      set.objfn(built.lp, c(PRICESO, rep(0, n_x), rep(0, n_t), 0))
      set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "=", "<="))
      set.rhs (built.lp, c(rep(0, n_x), rep(0, n_y), 1, 0))
    } else {
      if (rts == "ndrs") {
        built.lp <- make.lp(n_x + n_y + 2, n_y + n_x + n_t + 1)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(rep(0, n_y), diag(-1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ], 0))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(diag(-1, ncol = n_y, nrow = n_y)[j, ], rep(0, n_x), YREF[j, ], 0))
        }
        set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)))
        set.row(built.lp, n_x + n_y + 2, c(rep(0, n_x + n_y), rep(1, n_t), -1))
        set.objfn(built.lp, c(PRICESO, rep(0, n_x), rep(0, n_t), 0))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "=", ">="))
        set.rhs (built.lp, c(rep(0, n_x), rep(0, n_y), 1, 0))
      } else {
        built.lp <- make.lp(n_x + n_y + 1, n_y + n_x + n_t + 1)
        for (i in 1:n_x) {
          set.row(built.lp, i, c(rep(0, n_y), diag(-1, nrow = n_x, ncol = n_x)[i, ], XREF[i, ], 0))
        }
        for (j in 1:n_y) {
          set.row(built.lp, n_x + j, c(diag(-1, ncol = n_y, nrow = n_y)[j, ], rep(0, n_x), YREF[j, ], 0))
        }
        set.row(built.lp, n_x + n_y + 1, c(rep(0, n_y), PRICESI, rep(0, n_t + 1)))
        set.objfn(built.lp, c(PRICESO, rep(0, n_x), rep(0, n_t), 0))
        set.constr.type(built.lp, c(rep("<=", n_x), rep(">=", n_y), "="))
        set.rhs (built.lp, c(rep(0, n_x), rep(0, n_y), 1))
      }
    }
  }
  lp.control(built.lp, sense = "max")
  solve(built.lp)
  tfp <- get.objective(built.lp)
  tfp
}  #XOBS and YOBS are not actually useful for this computation. We keep it for the form

## dual shephard output distance function

DO.shdu <- function(XOBS, YOBS, XREF, YREF, rts) {
  n_x <- dim(XREF)[1]
  n_y <- dim(YREF)[1]
  n_t <- dim(XREF)[2]
  if (rts == "vrs") {
    built.lp <- make.lp(n_t + 1, n_y + n_x + 1)
    for (i in 1:n_y) {
      set.column(built.lp, i, c(-YREF[i, ], YOBS[i]))
    }
    for (j in 1:n_x) {
      set.column(built.lp, n_y + j, c(XREF[j, ], 0))
    }
    set.column(built.lp, n_x + n_y + 1, c(rep(1, n_t), 0))
    set.objfn(built.lp, c(rep(0, n_y), XOBS, 1))
    set.constr.type(built.lp, c(rep(">=", n_t), "="))
    set.rhs (built.lp, c(rep(0, n_t), 1))
    set.bounds(built.lp, lower = -Inf, upper = Inf, columns = n_x + n_y + 1)
    lp.control(built.lp, sense = "min")
    solve(built.lp)
    prices_o <- get.variables(built.lp)[1:n_y]/(sum(get.variables(built.lp)[(1 + n_y):(n_y + n_x)] * XOBS) + get.variables(built.lp)[n_y + n_x + 1])
  } else {
    if (rts == "nirs") {
      built.lp <- make.lp(n_t + 1, n_y + n_x + 1)
      for (i in 1:n_y) {
        set.column(built.lp, i, c(-YREF[i, ], YOBS[i]))
      }
      for (j in 1:n_x) {
        set.column(built.lp, n_y + j, c(XREF[j, ], 0))
      }
      set.column(built.lp, n_x + n_y + 1, c(rep(1, n_t), 0))
      set.objfn(built.lp, c(rep(0, n_y), XOBS, 1))
      set.constr.type(built.lp, c(rep(">=", n_t), "="))
      set.rhs (built.lp, c(rep(0, n_t), 1))
      lp.control(built.lp, sense = "min")
      solve(built.lp)
      prices_o <- get.variables(built.lp)[1:n_y]/(sum(get.variables(built.lp)[(1 + n_y):(n_y + n_x)] * XOBS) + get.variables(built.lp)[n_y + n_x + 1])
    } else {
      if (rts == "ndrs") {
        built.lp <- make.lp(n_t + 1, n_y + n_x + 1)
        for (i in 1:n_y) {
          set.column(built.lp, i, c(-YREF[i, ], YOBS[i]))
        }
        for (j in 1:n_x) {
          set.column(built.lp, n_y + j, c(XREF[j, ], 0))
        }
        set.column(built.lp, n_x + n_y + 1, c(rep(-1, n_t), 0))
        set.objfn(built.lp, c(rep(0, n_y), XOBS, -1))
        set.constr.type(built.lp, c(rep(">=", n_t), "="))
        set.rhs (built.lp, c(rep(0, n_t), 1))
        lp.control(built.lp, sense = "min")
        solve(built.lp)
        prices_o <- get.variables(built.lp)[1:n_y]/(sum(get.variables(built.lp)[(1 + n_y):(n_y + n_x)] * XOBS) - get.variables(built.lp)[n_y + n_x + 1])
      } else {
        built.lp <- make.lp(n_t + 1, n_y + n_x)
        for (i in 1:n_y) {
          set.column(built.lp, i, c(-YREF[i, ], YOBS[i]))
        }
        for (j in 1:n_x) {
          set.column(built.lp, n_y + j, c(XREF[j, ], 0))
        }
        set.objfn(built.lp, c(rep(0, n_y), XOBS))
        set.constr.type(built.lp, c(rep(">=", n_t), "="))
        set.rhs (built.lp, c(rep(0, n_t), 1))
        lp.control(built.lp, sense = "min", improve = "bbsimplex")
        solve(built.lp)
        prices_o <- get.variables(built.lp)[1:n_y]/(sum(get.variables(built.lp)[(1 + n_y):(n_y + n_x)] * XOBS))
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
    built.lp <- make.lp(n_t + 1, n_y + n_x + 1)
    for (i in 1:n_y) {
      set.column(built.lp, i, c(YREF[i, ], 0))
    }
    for (j in 1:n_x) {
      set.column(built.lp, n_y + j, c(-XREF[j, ], XOBS[j]))
    }
    set.column(built.lp, n_x + n_y + 1, c(rep(1, n_t), 0))
    set.objfn(built.lp, c(YOBS, rep(0, n_x), 1))
    set.constr.type(built.lp, c(rep("<=", n_t), "="))
    set.rhs (built.lp, c(rep(0, n_t), 1))
    set.bounds(built.lp, lower = -Inf, upper = Inf, columns = n_x + n_y + 1)
    lp.control(built.lp, sense = "max")
    solve(built.lp)
    prices_i <- get.variables(built.lp)[(n_y + 1):(n_y + n_x)]/(sum(get.variables(built.lp)[1:n_y] * YOBS) + 
                get.variables(built.lp)[n_x + n_y + 1])
  } else {
    if (rts == "nirs") {
      built.lp <- make.lp(n_t + 1, n_y + n_x + 1)
      for (i in 1:n_y) {
        set.column(built.lp, i, c(YREF[i, ], 0))
      }
      for (j in 1:n_x) {
        set.column(built.lp, n_y + j, c(-XREF[j, ], XOBS[j]))
      }
      set.column(built.lp, n_x + n_y + 1, c(rep(-1, n_t), 0))
      set.objfn(built.lp, c(YOBS, rep(0, n_x), -1))
      set.constr.type(built.lp, c(rep("<=", n_t), "="))
      set.rhs (built.lp, c(rep(0, n_t), 1))
      lp.control(built.lp, sense = "max")
      solve(built.lp)
      prices_i <- get.variables(built.lp)[(n_y + 1):(n_y + n_x)]/(sum(get.variables(built.lp)[1:n_y] * YOBS) - 
                  get.variables(built.lp)[n_x + n_y + 1])
    } else {
      if (rts == "ndrs") {
        built.lp <- make.lp(n_t + 1, n_y + n_x + 1)
        for (i in 1:n_y) {
          set.column(built.lp, i, c(YREF[i, ], 0))
        }
        for (j in 1:n_x) {
          set.column(built.lp, n_y + j, c(-XREF[j, ], XOBS[j]))
        }
        set.column(built.lp, n_x + n_y + 1, c(rep(1, n_t), 0))
        set.objfn(built.lp, c(YOBS, rep(0, n_x), 1))
        set.constr.type(built.lp, c(rep("<=", n_t), "="))
        set.rhs (built.lp, c(rep(0, n_t), 1))
        lp.control(built.lp, sense = "max")
        solve(built.lp)
        prices_i <- get.variables(built.lp)[(n_y + 1):(n_y + n_x)]/(sum(get.variables(built.lp)[1:n_y] * 
          YOBS) + get.variables(built.lp)[n_x + n_y + 1])
      } else {
        built.lp <- make.lp(n_t + 1, n_y + n_x)
        for (i in 1:n_y) {
          set.column(built.lp, i, c(YREF[i, ], 0))
        }
        for (j in 1:n_x) {
          set.column(built.lp, n_y + j, c(-XREF[j, ], XOBS[j]))
        }
        set.objfn(built.lp, c(YOBS, rep(0, n_x)))
        set.constr.type(built.lp, c(rep("<=", n_t), "="))
        set.rhs (built.lp, c(rep(0, n_t), 1))
        lp.control(built.lp, sense = "max")
        solve(built.lp)
        prices_i <- get.variables(built.lp)[(n_y + 1):(n_y + n_x)]/(sum(get.variables(built.lp)[1:n_y] * 
          YOBS))
      }
    }
  }
  names(prices_i) <- paste("V", 1:n_x, sep = "")
  prices_i
}

## apply a function to divide 2 columns
fdiv <- function(x) x[, 1]/x[, 2]

## Round-up all values
round.up <- function(x, n) sign(x) * trunc(abs(x) * 10^n + 0.5)/10^n

# integer check
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

## year vector for each spatial region when window is provided
lyear <- function(x, data, step1) {
  unique(data[, step1$time.var])[seq(1, length(unique(data[, step1$time.var])), by = x)]
}

## Test for balanced panel data
balanced <- function(data, id.var, time.var) {
  x <- data[, id.var]
  y <- data[, time.var]
  if (length(x) != length(y)) stop(paste0("The length of the two vectors (i.e. ", id.var, " and ", time.var, ") differs\n"))
  x <- data[, id.var][drop = TRUE]
  y <- data[, time.var][drop = TRUE]
  z <- table(x, y)
  if (any(as.vector(z) == 0)) {
    balanced <- FALSE
  } else {
    balanced <- TRUE
  }
  return(balanced)
}

## Return functions (i.e. Levels(); Changes(); Shadowp())
Levels <- function(object, ...) {
    if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche", "HicksMoorsteen"))) {
        stop("Function 'Levels' can not be applied to an object of class \"", class(object), "\"")
    }
    return(object$Levels)
}

Changes <- function(object, ...) {
    if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Malmquist", "Paasche", "HicksMoorsteen"))) {
        stop("Function 'Changes' can not be applied to an object of class \"", class(object), "\"")
    }
    return(object$Changes)
}

Shadowp <- function(object, ...) {
    if (is(object, c("Malmquist"))) {
        stop("Function 'Shadowp' can not be applied to an object of class \"", class(object)[2], "\"")
    }
    if (!is(object, c("FarePrimont", "Fisher", "Laspeyres", "Lowe", "Paasche", "HicksMoorsteen"))) {
        stop("Function 'Shadowp' can not be applied to an object of class \"", class(object), "\"")
    }
    if (is.null(object$Shadowp)) {
        stop("No shadow prices are returned in you \"", class(object)[2], "\"", " object. 
Specifying 'shadow = TRUE' should be considered in the function generating the \"", class(object)[2], "\"", " object.")
    }
    return(object$Shadowp)
}