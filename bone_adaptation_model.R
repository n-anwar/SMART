# =============================================================================
# bone_adaptation_model.R
#
# R translation of bone_adaptation_model.jl
# =============================================================================

library(deSolve)


# =============================================================================
# 1.  Default model parameters  (Table 9 of the paper)
#     These match the Julia defaults exactly.
#     For simulations, use p_optimal() to override A_OBL_max, A_OCL_max, sigma0.
# =============================================================================

default_params <- list(
  aSv       = 11.42,      # Fitting parameter relating fBM to Sv  [mm^-1]
  bSv       = -0.02,      # Fitting parameter relating fBM to Sv  [-]
  E0        = 2500,       # Maximum bone stiffness                 [MPa]
  gamma_E   = 0.06,       # Exponent of strain rate in stiffness   [-]
  alpha     = 0.19,       # Fraction of specific surface           [-]
  Fs        = 5,          # Damage repair specificity factor       [-]
  A_OBL_min = 0.001094,   # Minimum bone formation rate            [mm/d]
  A_OBL_max = 0.0127,     # Maximum bone formation rate (default)  [mm/d]
  delta_B   = 7,          # Half-saturation stress, formation      [MPa]
  gamma_B   = 1,          # Sigmoidicity of bone formation         [-]
  A_OCL_min = 0.001,      # Minimum bone resorption rate           [mm/d]
  A_OCL_max = 0.011,      # Maximum bone resorption rate (default) [mm/d]
  delta_C   = 1,          # Half-saturation stress, resorption     [MPa]
  gamma_C   = 2,          # Sigmoidicity of bone resorption        [-]
  sigma0    = 134.2,      # Fatigue-life regression coefficient    [MPa]
  sigma1    = 14.1,       # Fatigue-life regression coefficient    [MPa]
  E_nominal = NA          # Computed below
)

# E_nominal: reference stiffness at fBM = 0.9, strain_rate = 0.36
# Julia: E_nominal_default = E0 * 0.9^3 * 0.36^gamma_E
default_params$E_nominal <- with(default_params,
  E0 * 0.9^3 * 0.36^gamma_E
)


# =============================================================================
# 2.  Optimal fitted parameters
#     Mirrors Julia's p_optimal() function.
#     These override A_OBL_max, A_OCL_max and sigma0 in default_params
#     and are used for all simulations in the manuscript.
# =============================================================================

p_optimal <- function() {
  list(
    A_OBL_max = 0.006028609291464905,
    A_OCL_max = 0.003582042256313746,
    sigma0    = 139.02301150354072
  )
}

# Build simulation parameters: default_params with optimal values applied
# Julia: ps = p_optimal() passed to ODEProblem
sim_params <- modifyList(default_params, p_optimal())


# =============================================================================
# 3.  Biomechanical helper functions
# =============================================================================

# Specific surface density as a function of bone volume fraction
# Eq. (A2) - Hitchens et al. (2018)
surface_density <- function(fBM, aSv = default_params$aSv,
                                  bSv = default_params$bSv) {
  aSv * sqrt(pmax(1 - fBM, 0)) * (bSv * fBM + 1 - bSv)
}

# Bone stiffness (Young's modulus) [MPa]
# Eq. (A6)
loading_stiffness <- function(fBM, strain_rate,
                              E0      = default_params$E0,
                              gamma_E = default_params$gamma_E) {
  E0 * fBM^3 * strain_rate^gamma_E
}

# Strain energy density [MPa]
# Eq. (A5)
strain_energy_density <- function(sigma, E) {
  0.5 / E * sigma^2
}

# Osteoblast activity [mm/d]
# Eq. (A3) - Hill activation function (increases with mechanical stimulus)
osteoblast_activity <- function(psi,
                                A_min = default_params$A_OBL_min,
                                A_max = default_params$A_OBL_max,
                                delta = default_params$delta_B,
                                gamma = default_params$gamma_B) {
  A_min + (A_max - A_min) * psi^gamma / (delta^gamma + psi^gamma)
}

# Osteoclast activity [mm/d]
# Eq. (A4) - Hill inhibition function (decreases with mechanical stimulus)
osteoclast_activity <- function(psi,
                                A_min = default_params$A_OCL_min,
                                A_max = default_params$A_OCL_max,
                                delta = default_params$delta_C,
                                gamma = default_params$gamma_C) {
  A_min + (A_max - A_min) * delta^gamma / (delta^gamma + psi^gamma)
}

# Fatigue life: number of loading cycles to failure [-]
# Julia: fatigue_life(sigma; sigma0, sigma1) = 10^(-(sigma - sigma0)/sigma1)
fatigue_life <- function(sigma,
                         sigma0 = default_params$sigma0,
                         sigma1 = default_params$sigma1) {
  10^(-(sigma - sigma0) / sigma1)
}

# Damage accumulated per stride at a given stress
# Julia: damage_rate_eq -> dmg_rate ~ 1/fatigue_life(sigma)

# damage_per_stride <- function(sigma,
#                               sigma0 = default_params$sigma0,
#                               sigma1 = default_params$sigma1) {
#   1 / fatigue_life(sigma, sigma0, sigma1)
# }


# =============================================================================
# 4.  Speed-to-mechanics conversion functions
#     Eqs. (A11)-(A15)
# =============================================================================

# Vertical (ground reaction) force [N / kg body weight]
# Julia: vertical_force(speed) = 2.778 + 2.1376*speed - 0.0535*speed^2
vertical_force <- function(speed) {
  2.778 + 2.1376 * speed - 0.0535 * speed^2
}

# Joint stress [MPa]
# Eq. (A12): sigma = sigma_max * Fg / Fmax
joint_stress <- function(speed, scaled_load = 90) {
  scaling_factor <- scaled_load / 24.13
  scaling_factor * vertical_force(speed)
}

# Strain rate [s^-1]
# Eq. (A13)
strain_rate_fitted <- function(speed) {
  0.015481010197084148 * speed + 0.0004955110898565426 * speed^2
}

# Stride frequency [Hz]
# Eq. (A14)
stride_frequency <- function(speed) {
  1.7052 + 0.0305 * speed + 0.0004 * speed^2
}

# Stride length [m]
# Julia: stride_length(speed) = speed / stride_frequency(speed)
stride_length <- function(speed) {
  speed / stride_frequency(speed)
}

# Number of loading cycles per day
# Eq. (A15): vn = d * f / s
cycles_per_day <- function(distance_per_day, speed) {
  distance_per_day * stride_frequency(speed) / speed
}

# Damage from a single bout: distance at a fixed speed
# Julia: damage(distance, speed; sigma0) = n_strides / fatigue_life(joint_stress(speed))
damage_from_distance <- function(distance, speed,
                                 sigma0 = default_params$sigma0) {
  n_strides <- distance / stride_length(speed)
  n_strides / fatigue_life(joint_stress(speed), sigma0 = sigma0)
}


# =============================================================================
# 5.  Steady-state calculations
# =============================================================================

# Steady-state strain energy density [MPa]
# Found by solving A_OBL(psi) = A_OCL(psi), i.e. f(psi) = 0.
#
# Derivation: at steady state d(fBM)/dt = 0, so (A_OBL - A_OCL)*alpha*Sv = 0.
# Since alpha > 0 and Sv > 0, we need A_OBL(psi) = A_OCL(psi).
# Multiplying A_OBL(psi) - A_OCL(psi) = 0 through by (delta_B + psi)*(delta_C^2 + psi^2)
# clears denominators term by term (each Hill fraction cancels only its own
# denominator), yielding a cubic in psi with coefficients (gamma_B=1, gamma_C=2):
#
#   c3 = A_OBL_max - A_OCL_min
#   c2 = (A_OBL_min - A_OCL_min) * delta_B
#   c1 = delta_C^2 * (A_OBL_max - A_OCL_max)
#   c0 = delta_B * delta_C^2 * (A_OBL_min - A_OCL_max)
#
# Rather than solving analytically, uniroot() finds the unique positive
# root directly from f(psi), which works for any gamma_B and gamma_C.
sed_ss <- function(A_OBL_min = default_params$A_OBL_min,
                   A_OBL_max = default_params$A_OBL_max,
                   delta_B   = default_params$delta_B,
                   gamma_B   = default_params$gamma_B,
                   A_OCL_min = default_params$A_OCL_min,
                   A_OCL_max = default_params$A_OCL_max,
                   delta_C   = default_params$delta_C,
                   gamma_C   = default_params$gamma_C) {
  f <- function(psi) {
    osteoblast_activity(psi, A_OBL_min, A_OBL_max, delta_B, gamma_B) -
    osteoclast_activity(psi, A_OCL_min, A_OCL_max, delta_C, gamma_C)
  }
  tryCatch(
    uniroot(f, interval = c(0.001, 50))$root,
    error = function(e) {
      warning("sed_ss: could not find root in (0.001, 50). Check parameters.")
      NA_real_
    }
  )
}

# Steady-state bone volume fraction for a given stress and strain rate
# Derived from psi_ss = sigma^2 / (2 * E0 * strain_rate^gamma_E * fBM^3)
# rearranged for fBM:
#   fBM_ss = (sigma^2 / (2 * E0 * strain_rate^gamma_E * psi_ss))^(1/3)
fBM_ss <- function(sigma, strain_rate, params = sim_params) {
  psi_ss <- sed_ss(
    A_OBL_min = params$A_OBL_min,
    A_OBL_max = params$A_OBL_max,
    delta_B   = params$delta_B,
    gamma_B   = params$gamma_B,
    A_OCL_min = params$A_OCL_min,
    A_OCL_max = params$A_OCL_max,
    delta_C   = params$delta_C,
    gamma_C   = params$gamma_C
  )
  (sigma^2 / (2 * params$E0 * strain_rate^params$gamma_E * psi_ss))^(1/3)
}

# Initial bone volume fraction: steady state at trot speed (3.6 m/s)
# Mirrors the inline computation in Julia's simulate_program():
#   fBM0 = fBM_ss(joint_stress(3.6), strain_rate_fitted(3.6), A_OBL_max=ps[1][2], ...)
# Uses sim_params (optimal) by default, matching Julia exactly.
initial_fBM <- function(params = sim_params) {
  fBM_ss(
    sigma       = joint_stress(3.6),
    strain_rate = strain_rate_fitted(3.6),
    params      = params
  )
}

# Steady-state osteoblast activity at equilibrium SED
# Julia: A_ss() = osteoblast_activity(SED_ss())
# Not used in simulations; useful for diagnostics.
A_ss <- function(params = default_params) {
  psi_ss <- sed_ss(
    A_OBL_min = params$A_OBL_min,
    A_OBL_max = params$A_OBL_max,
    delta_B   = params$delta_B,
    gamma_B   = params$gamma_B,
    A_OCL_min = params$A_OCL_min,
    A_OCL_max = params$A_OCL_max,
    delta_C   = params$delta_C,
    gamma_C   = params$gamma_C
  )
  osteoblast_activity(psi_ss, params$A_OBL_min, params$A_OBL_max,
                      params$delta_B, params$gamma_B)
}


# =============================================================================
# 6.  ODE right-hand side
#
#     State vector: y = c(fBM, dmg)
#
#     The Julia code uses damage_input=true mode, where the damage formation
#     rate is passed directly as an input function (from damagefunc(tp)) rather
#     than being computed from n_strides and sigma inside the ODE.
#     This is the mode used in all manuscript simulations via ODESystem(tp).
#
#     inputs is a named list with:
#       inputs$sigma(t)                  - joint stress [MPa]
#       inputs$strain_rate(t)            - strain rate [s^-1]
#       inputs$damage_formation_rate(t)  - precomputed damage rate [d^-1]
#                                          (replaces n_strides * dmg_rate)
# =============================================================================

bone_ode <- function(t, y, parms, inputs) {
  fBM <- max(y[1], 1e-6)   # clamp to avoid sqrt(negative)
  dmg <- y[2]

  # Resolve mechanical inputs at time t
  sigma       <- inputs$sigma(t)
  strain_rate <- inputs$strain_rate(t)

  # Algebraic quantities
  Sv    <- surface_density(fBM, parms$aSv, parms$bSv)
  E     <- loading_stiffness(fBM, strain_rate, parms$E0, parms$gamma_E)
  psi   <- strain_energy_density(sigma, E)
  A_OBL <- osteoblast_activity(psi, parms$A_OBL_min, parms$A_OBL_max,
                               parms$delta_B, parms$gamma_B)
  A_OCL <- osteoclast_activity(psi, parms$A_OCL_min, parms$A_OCL_max,
                               parms$delta_C, parms$gamma_C)

  # Bone volume fraction ODE - Eq. (A1)
  d_fBM <- (A_OBL - A_OCL) * parms$alpha * Sv

  # Damage ODE - Eq. (A7)
  # Formation: supplied directly from damagefunc(tp) via build_ode_inputs()
  # Julia: D_formation ~ f_Dr(t)   [damage_input=true mode, no E scaling]
  # D_formation <- inputs$damage_formation_rate(t) ## this produce result that only matches the fBM
  
  D_formation <- inputs$damage_formation_rate(t) * (parms$E_nominal / E)
  
  # Repair: Eq. (A10)
  D_repair <- (A_OBL + (parms$Fs - 1) * A_OCL) * parms$alpha * Sv * dmg / fBM
  
  d_dmg <- D_formation - D_repair
  
  list(c(d_fBM, d_dmg))
}


# =============================================================================
# 7.  Solver
#
#     solve_bone_model() integrates the ODE over n_seasons repetitions of
#     the training programme cycle using deSolve::ode() with LSODA.
#
#     inputs: named list from build_ode_inputs(tp) in detailed_training_program.R
#       $sigma(t)                 - joint stress function
#       $strain_rate(t)           - strain rate function
#       $damage_formation_rate(t) - damage rate function
#
#     Returns a data.frame with columns: time, fBM, dmg
# =============================================================================

######with termination
# solve_bone_model <- function(inputs,
#                              y0,
#                              t_end,
#                              params  = sim_params,
#                              t_start = 0,
#                              t_out   = NULL) {
#   if (is.null(t_out)) {
#     t_out <- seq(t_start, t_end, by = 0.5)
#     if (tail(t_out, 1) < t_end) t_out <- c(t_out, t_end)
#   }
# 
#   # Termination event: stop when damage reaches 1
#   # Julia: termination_callback stops the integrator when dmg - 1 = 0
#   root_fn <- function(t, y, parms, ...) c(y[2] - 1)
# 
#   out <- tryCatch(
#     ode(
#       y       = c(fBM = y0[1], dmg = y0[2]),
#       times   = t_out,
#       func    = bone_ode,
#       parms   = params,
#       inputs  = inputs,
#       method  = "lsoda",
#       rtol    = 1e-9,
#       atol    = 1e-12,
#       rootfun = root_fn,
#       events  = list(root = TRUE, terminalroot = 1)
#     ),
#     error = function(e) {
#       # Fallback without root-finding
#       message("Root-finding termination not supported; running without early stop.")
#       ode(
#         y      = c(fBM = y0[1], dmg = y0[2]),
#         times  = t_out,
#         func   = bone_ode,
#         parms  = params,
#         inputs = inputs,
#         method = "lsoda",
#         rtol   = 1e-9,
#         atol   = 1e-12
#       )
#     }
#   )
# 
#   df <- as.data.frame(out)
#   names(df) <- c("time", "fBM", "dmg")
# 
#   # Manual trim after damage threshold as fallback
#   first_over <- which(df$dmg >= 1)
#   if (length(first_over) > 0) {
#     df <- df[seq_len(first_over[1]), ]
#   }
# 
#   df
# }

######without termination

solve_bone_model <- function(inputs,
                             y0,
                             t_end,
                             params  = sim_params,
                             t_start = 0,
                             t_out   = NULL) {
  if (is.null(t_out)) {
    t_out <- seq(t_start, t_end, by = 0.1)
    if (tail(t_out, 1) < t_end) t_out <- c(t_out, t_end)
  }
  
  out <- ode(
    y      = c(fBM = y0[1], dmg = y0[2]),
    times  = t_out,
    func   = bone_ode,
    parms  = params,
    inputs = inputs,
    method = "lsoda",
    rtol   = 1e-9,
    atol   = 1e-12
  )
  
  df <- as.data.frame(out)
  names(df) <- c("t", "fBM", "dmg")
  df
}