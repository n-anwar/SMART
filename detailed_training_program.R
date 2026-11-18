# =============================================================================
# detailed_training_program.R
#
# R translation of detailed_training_program.jl
#
# Constructs training programme objects and their associated speed/distance/
# damage functions, mirroring the Julia struct-based design using R lists
# and closures.
#
# Julia uses structs (HomogeneousProgram, SlowProgressiveProgram, etc.) and
# multiple dispatch. R has no native equivalent, so each "struct" becomes a
# named list with a class attribute, and dispatch is handled via S3 methods
# or explicit if/else on class.
#
# Source this file after bone_adaptation_model.R:
#   source("bone_adaptation_model.R")
#   source("detailed_training_program.R")
# =============================================================================

source("bone_adaptation_model.R")


# =============================================================================
# 1.  Helper functions
#     pulse() and ramp() are used throughout to define piecewise-linear
#     speed and distance profiles.
# =============================================================================

# pulse(x, a, b): returns 1 when a < x <= b, 0 otherwise
# Julia: pulse(x,a,b) = (a<x)*(x<=b)
pulse <- function(x, a, b) as.numeric(x > a & x <= b)

# ramp(x, a, b): returns (x - a) when a < x <= b, 0 otherwise
# Julia: ramp(x,a,b) = (x-a)*pulse(x,a,b)
ramp <- function(x, a, b) (x - a) * pulse(x, a, b)

# Fitted sigma0 from p_optimal()
sigma0_fitted <- p_optimal()$sigma0

# adjusted_damage(): damage formation rate from a distance covered at a speed.
# Speed is clamped to minimum 1.6 m/s to avoid numerical issues at very low speeds.
# Julia: adjusted_damage(distance,speed,σ0=σ0_fitted) = damage(distance,max(1.6,speed),σ0=σ0)
adjusted_damage <- function(distance, speed, sigma0 = sigma0_fitted) {
  damage_from_distance(distance, max(1.6, speed), sigma0 = sigma0)
}


# =============================================================================
# 2.  HomogeneousProgram
#     Constant speed and distance throughout the phase duration.
#     Julia struct: HomogeneousProgram(duration, distance_per_day, speed)
# =============================================================================

HomogeneousProgram <- function(duration, distance_per_day, speed) {
  structure(
    list(
      duration         = duration,
      distance_per_day = distance_per_day,
      speed            = speed
    ),
    class = "HomogeneousProgram"
  )
}

# duration method
duration.HomogeneousProgram <- function(hp) hp$duration

# speedfunc: returns a function of local time t giving speed within the phase
# Julia: speedfunc(hp) = t -> hp.speed * pulse(t, 0, duration(hp))
speedfunc.HomogeneousProgram <- function(hp) {

  function(t) hp$speed * pulse(t, 0, hp$duration)
}

# distancefunc: returns a function of local time t giving distance per day
# Julia: distancefunc(hp) = t -> hp.distance_per_day * pulse(t, 0, duration(hp))
distancefunc.HomogeneousProgram <- function(hp) {
  function(t) hp$distance_per_day * pulse(t, 0, hp$duration)
}

# damagefunc: returns a function of local time t giving damage formation rate
# Julia: damagefunc(hp; σ0) = t -> adjusted_damage(distancefunc(hp)(t), speedfunc(hp)(t), σ0)
damagefunc.HomogeneousProgram <- function(hp, sigma0 = sigma0_fitted) {
  df <- distancefunc(hp)
  sf <- speedfunc(hp)
  function(t) adjusted_damage(df(t), sf(t), sigma0)
}


# =============================================================================
# 3.  SlowProgressiveProgram
#     Speed and distance increase linearly over the phase duration.
#     Julia struct: SlowProgressiveProgram(duration, dist_start, dist_end,
#                                          speed_start, speed_end)
# =============================================================================

SlowProgressiveProgram <- function(duration,
                                   distance_per_day_start,
                                   distance_per_day_end,
                                   speed_start,
                                   speed_end) {
  structure(
    list(
      duration               = duration,
      distance_per_day_start = distance_per_day_start,
      distance_per_day_end   = distance_per_day_end,
      speed_start            = speed_start,
      speed_end              = speed_end
    ),
    class = "SlowProgressiveProgram"
  )
}

duration.SlowProgressiveProgram <- function(spp) spp$duration

# speedfunc: linear ramp from speed_start to speed_end
# Julia: speedfunc(spp) = t -> spp.speed_start*pulse(t,0,dur) +
#                               (speed_end-speed_start)/dur * ramp(t,0,dur)
speedfunc.SlowProgressiveProgram <- function(spp) {
  function(t) {
    spp$speed_start * pulse(t, 0, spp$duration) +
      (spp$speed_end - spp$speed_start) / spp$duration * ramp(t, 0, spp$duration)
  }
}

# distancefunc: linear ramp from distance_start to distance_end
# Julia: distancefunc(spp) = t -> dist_start*pulse(t,0,dur) +
#                                  (dist_end-dist_start)/dur * ramp(t,0,dur)
distancefunc.SlowProgressiveProgram <- function(spp) {
  function(t) {
    spp$distance_per_day_start * pulse(t, 0, spp$duration) +
      (spp$distance_per_day_end - spp$distance_per_day_start) / spp$duration * ramp(t, 0, spp$duration)
  }
}

damagefunc.SlowProgressiveProgram <- function(spp, sigma0 = sigma0_fitted) {
  df <- distancefunc(spp)
  sf <- speedfunc(spp)
  function(t) adjusted_damage(df(t), sf(t), sigma0)
}


# =============================================================================
# 4.  FastProgressiveProgram
#     Two concurrent speeds: speed1 (slow gallop, 13.8 m/s) starts with the
#     full distance and is progressively replaced by speed2 (fast gallop,
#     16.0 m/s) which grows linearly from zero (triangle rule).
#
#     Julia struct: FastProgressiveProgram(duration, cumulative_distance1,
#                                          cumulative_distance2, speed1, speed2)
# =============================================================================

FastProgressiveProgram <- function(duration,
                                   cumulative_distance1,
                                   cumulative_distance2,
                                   speed1,
                                   speed2) {
  structure(
    list(
      duration             = duration,
      cumulative_distance1 = cumulative_distance1,
      cumulative_distance2 = cumulative_distance2,
      speed1               = speed1,
      speed2               = speed2
    ),
    class = "FastProgressiveProgram"
  )
}

duration.FastProgressiveProgram <- function(fpp) fpp$duration

# Total distance per day (constant throughout the fast phase)
# Julia: total_distance(fpp) = (cum_dist1 + cum_dist2) / duration
total_distance.FastProgressiveProgram <- function(fpp) {
  (fpp$cumulative_distance1 + fpp$cumulative_distance2) / fpp$duration
}

# Final per-day distance at speed2 (using triangle rule: area = base*height/2)
# Julia: final_distance2(fpp) = cumulative_distance2 * 2 / duration
final_distance2.FastProgressiveProgram <- function(fpp) {
  fpp$cumulative_distance2 * 2 / fpp$duration
}

# distancefunc2: distance at speed2 grows linearly (ramp)
# Julia: distancefunc2(fpp) = t -> final_distance2(fpp)/duration * ramp(t,0,duration)
distancefunc2.FastProgressiveProgram <- function(fpp) {
  fd2    <- final_distance2.FastProgressiveProgram(fpp)
  function(t) fd2 / fpp$duration * ramp(t, 0, fpp$duration)
}

# distancefunc1: total distance minus distance at speed2
# Julia: distancefunc1(fpp) = t -> total_distance(fpp)*pulse(t,0,dur) - distancefunc2(fpp)(t)
distancefunc1.FastProgressiveProgram <- function(fpp) {
  td    <- total_distance.FastProgressiveProgram(fpp)
  df2   <- distancefunc2.FastProgressiveProgram(fpp)
  function(t) td * pulse(t, 0, fpp$duration) - df2(t)
}

# distancefunc: total = dist1 + dist2
distancefunc.FastProgressiveProgram <- function(fpp) {
  df1 <- distancefunc1.FastProgressiveProgram(fpp)
  df2 <- distancefunc2.FastProgressiveProgram(fpp)
  function(t) df1(t) + df2(t)
}

# speedfunc: maximum of the two speeds (highest speed drives bone stimulus)
# Julia: speedfunc(fpp) = t -> max(speed1*pulse(...), speed2*pulse(...))
speedfunc.FastProgressiveProgram <- function(fpp) {
  function(t) max(fpp$speed1 * pulse(t, 0, fpp$duration), fpp$speed2 * pulse(t, 0, fpp$duration))
}

# damagefunc1 and damagefunc2: damage from each speed component
damagefunc1.FastProgressiveProgram <- function(fpp, sigma0 = sigma0_fitted) {
  df1 <- distancefunc1.FastProgressiveProgram(fpp)

  function(t) adjusted_damage(df1(t), fpp$speed1 * pulse(t, 0, fpp$duration), sigma0)
}

damagefunc2.FastProgressiveProgram <- function(fpp, sigma0 = sigma0_fitted) {
  df2 <- distancefunc2.FastProgressiveProgram(fpp)
  function(t) adjusted_damage(df2(t), fpp$speed2 * pulse(t, 0, fpp$duration), sigma0)
}

# damagefunc: sum of damage from both speed components
# Julia: damagefunc(fpp) = t -> damagefunc1(fpp)(t) + damagefunc2(fpp)(t)
damagefunc.FastProgressiveProgram <- function(fpp, sigma0 = sigma0_fitted) {
  df1 <- damagefunc1.FastProgressiveProgram(fpp, sigma0)
  df2 <- damagefunc2.FastProgressiveProgram(fpp, sigma0)
  function(t) df1(t) + df2(t)
}


# =============================================================================
# 5.  RacefitProgram
#     Three concurrent components: slow gallop, fast gallop, and racing.
#     Each is internally a HomogeneousProgram.
#     Julia struct: RacefitProgram(duration, slow_gallop_dist, fast_gallop_dist,
#                                  race_dist, slow_gallop_speed, fast_gallop_speed,
#                                  race_speed)
# =============================================================================

RacefitProgram <- function(duration,
                           slow_gallop_distance_per_day,
                           fast_gallop_distance_per_day,
                           race_distance_per_day,
                           slow_gallop_speed,
                           fast_gallop_speed,
                           race_speed) {
  structure(
    list(
      duration                    = duration,
      slow_gallop_distance_per_day = slow_gallop_distance_per_day,
      fast_gallop_distance_per_day = fast_gallop_distance_per_day,
      race_distance_per_day        = race_distance_per_day,
      slow_gallop_speed            = slow_gallop_speed,
      fast_gallop_speed            = fast_gallop_speed,
      race_speed                   = race_speed
    ),
    class = "RacefitProgram"
  )
}

duration.RacefitProgram <- function(rp) rp$duration

# Internal sub-programs (each is a HomogeneousProgram)
slowgallop_program  <- function(rp) HomogeneousProgram(rp$duration, rp$slow_gallop_distance_per_day, rp$slow_gallop_speed)
fastgallop_program  <- function(rp) HomogeneousProgram(rp$duration, rp$fast_gallop_distance_per_day, rp$fast_gallop_speed)
race_program        <- function(rp) HomogeneousProgram(rp$duration, rp$race_distance_per_day,        rp$race_speed)

# speedfunc: racing speed is the dominant speed for bone stimulus
# Julia: speedfunc(rp) = t -> rp.race_speed * pulse(t, 0, duration(rp))
speedfunc.RacefitProgram <- function(rp) {
  function(t) rp$race_speed * pulse(t, 0, rp$duration)
}

# distancefuncs for each component
slowgallop_distancefunc <- function(rp) distancefunc(slowgallop_program(rp))
fastgallop_distancefunc <- function(rp) distancefunc(fastgallop_program(rp))
race_distancefunc_rf    <- function(rp) distancefunc(race_program(rp))

# distancefunc: sum of all three components
# Julia: distancefunc(rp) = t -> slow(t) + fast(t) + race(t)
distancefunc.RacefitProgram <- function(rp) {
  df_slow <- slowgallop_distancefunc(rp)
  df_fast <- fastgallop_distancefunc(rp)
  df_race <- race_distancefunc_rf(rp)
  function(t) df_slow(t) + df_fast(t) + df_race(t)
}

# damagefuncs for each component
slowgallop_damagefunc <- function(rp, sigma0 = sigma0_fitted) damagefunc(slowgallop_program(rp), sigma0)
fastgallop_damagefunc <- function(rp, sigma0 = sigma0_fitted) damagefunc(fastgallop_program(rp), sigma0)
race_damagefunc       <- function(rp, sigma0 = sigma0_fitted) damagefunc(race_program(rp),        sigma0)

# damagefunc: sum of all three
# Julia: damagefunc(rp) = t -> slow_dmg(t) + fast_dmg(t) + race_dmg(t)
damagefunc.RacefitProgram <- function(rp, sigma0 = sigma0_fitted) {
  df_slow <- slowgallop_damagefunc(rp, sigma0)
  df_fast <- fastgallop_damagefunc(rp, sigma0)
  df_race <- race_damagefunc(rp, sigma0)
  function(t) df_slow(t) + df_fast(t) + df_race(t)
}


# =============================================================================
# 6.  S3 generics
#     Define generic functions so the methods above dispatch correctly.
# =============================================================================

duration     <- function(x, ...) UseMethod("duration")
speedfunc    <- function(x, ...) UseMethod("speedfunc")
distancefunc <- function(x, ...) UseMethod("distancefunc")
damagefunc   <- function(x, ...) UseMethod("damagefunc")


# =============================================================================
# 7.  TrainingProgram
#     The full preparation cycle: rest -> pre-training -> slow progressive ->
#     fast progressive -> race-fit, with slow workouts running in parallel
#     during the progressive and race-fit phases.
#
#     Julia struct: TrainingProgram(rest, pretraining, slow_progressive,
#                                   fast_progressive, racefit, slow_workout)
# =============================================================================

TrainingProgram <- function(rest_program,
                            pretraining_program,
                            slow_progressive_program,
                            fast_progressive_program,
                            racefit_program,
                            slow_workout) {
  structure(
    list(
      rest_program             = rest_program,
      pretraining_program      = pretraining_program,
      slow_progressive_program = slow_progressive_program,
      fast_progressive_program = fast_progressive_program,
      racefit_program          = racefit_program,
      slow_workout             = slow_workout
    ),
    class = c("TrainingProgram", "AbstractRepeatedProgram")
  )
}

# --- Phase duration helpers ---
progressive_program_duration <- function(tp) {
  duration(tp$slow_progressive_program) + duration(tp$fast_progressive_program)
}

duration.TrainingProgram <- function(tp) {
  duration(tp$rest_program) + duration(tp$pretraining_program) +
    progressive_program_duration(tp) + duration(tp$racefit_program)
}

# --- Phase start times (all relative to the start of the cycle) ---
# Julia: rest_start_time = 0
rest_start_time <- function(tp) 0

# Julia: pretraining_start_time = duration(rest)
pretraining_start_time <- function(tp) duration(tp$rest_program)

# Julia: slow_progressive_start_time = pretraining_start + duration(pretraining)
slow_progressive_start_time <- function(tp) {
  pretraining_start_time(tp) + duration(tp$pretraining_program)
}

# Julia: fast_progressive_start_time = slow_progressive_start + duration(slow_progressive)
fast_progressive_start_time <- function(tp) {
  slow_progressive_start_time(tp) + duration(tp$slow_progressive_program)
}

# Julia: racefit_start_time = fast_progressive_start + duration(fast_progressive)
racefit_start_time <- function(tp) {
  fast_progressive_start_time(tp) + duration(tp$fast_progressive_program)
}

# --- speedfunc for the full programme ---
# Each phase's speed function is evaluated at local time (t minus phase start).
# The pre-rest period (t < 0) uses rest speed, mirroring Julia's pulse(t,-Inf,0).
# Julia: speedfunc(tp) = t -> rest_speed*pulse(t,-Inf,0) + sum of phase speedfuncs
speedfunc.TrainingProgram <- function(tp) {
  t_pretrain  <- pretraining_start_time(tp)
  t_slowprog  <- slow_progressive_start_time(tp)
  t_fastprog  <- fast_progressive_start_time(tp)
  t_racefit   <- racefit_start_time(tp)
  rest_spd    <- tp$rest_program$speed
  sf_rest     <- speedfunc(tp$rest_program)
  sf_pretrain <- speedfunc(tp$pretraining_program)
  sf_slowprog <- speedfunc(tp$slow_progressive_program)
  sf_fastprog <- speedfunc(tp$fast_progressive_program)
  sf_racefit  <- speedfunc(tp$racefit_program)
  function(t) {
    rest_spd * pulse(t, -Inf, 0) +
      sf_rest(t) +
      sf_pretrain(t - t_pretrain) +
      sf_slowprog(t - t_slowprog) +
      sf_fastprog(t - t_fastprog) +
      sf_racefit(t  - t_racefit)
  }
}

# --- Per-phase distance functions (each takes global time t) ---
rest_distancefunc <- function(tp) {
  t0 <- rest_start_time(tp)
  df <- distancefunc(tp$rest_program)
  function(t) df(t - t0)
}
pretraining_distancefunc <- function(tp) {
  t0 <- pretraining_start_time(tp)
  df <- distancefunc(tp$pretraining_program)
  function(t) df(t - t0)
}
slowprogressive_distancefunc <- function(tp) {
  t0 <- slow_progressive_start_time(tp)
  df <- distancefunc(tp$slow_progressive_program)
  function(t) df(t - t0)
}
fastprogressive_distancefunc <- function(tp) {
  t0 <- fast_progressive_start_time(tp)
  df <- distancefunc(tp$fast_progressive_program)
  function(t) df(t - t0)
}
racefit_distancefunc <- function(tp) {
  t0 <- racefit_start_time(tp)
  df <- distancefunc(tp$racefit_program)
  function(t) df(t - t0)
}

# Slow workout runs from slow_progressive_start through the end of race-fit
slowworkout_distancefunc <- function(tp) {
  t0 <- slow_progressive_start_time(tp)
  df <- distancefunc(tp$slow_workout)
  function(t) df(t - t0)
}

# distancefunc for the full programme
# Julia: distancefunc(tp) = t -> rest_dpd*pulse(t,-Inf,0) + sum of all phase distancefuncs
distancefunc.TrainingProgram <- function(tp) {
  rest_dpd    <- tp$rest_program$distance_per_day
  df_rest     <- rest_distancefunc(tp)
  df_pretrain <- pretraining_distancefunc(tp)
  df_slowprog <- slowprogressive_distancefunc(tp)
  df_fastprog <- fastprogressive_distancefunc(tp)
  df_racefit  <- racefit_distancefunc(tp)
  df_slowwrkt <- slowworkout_distancefunc(tp)
  function(t) {
    rest_dpd * pulse(t, -Inf, 0) +
      df_rest(t) + df_pretrain(t) + df_slowprog(t) +
      df_fastprog(t) + df_racefit(t) + df_slowwrkt(t)
  }
}

# --- Per-phase damage functions ---
rest_damagefunc <- function(tp, sigma0 = sigma0_fitted) {
  t0 <- rest_start_time(tp)
  df <- damagefunc(tp$rest_program, sigma0)
  function(t) df(t - t0)   # s0 kept for interface consistency
}
pretraining_damagefunc <- function(tp, sigma0 = sigma0_fitted) {
  t0 <- pretraining_start_time(tp)
  df <- damagefunc(tp$pretraining_program, sigma0)
  function(t) df(t - t0)
}
slowprogressive_damagefunc <- function(tp, sigma0 = sigma0_fitted) {
  t0 <- slow_progressive_start_time(tp)
  df <- damagefunc(tp$slow_progressive_program, sigma0)
  function(t) df(t - t0)
}
fastprogressive_damagefunc <- function(tp, sigma0 = sigma0_fitted) {
  t0 <- fast_progressive_start_time(tp)
  df <- damagefunc(tp$fast_progressive_program, sigma0)
  function(t) df(t - t0)
}
racefit_damagefunc_tp <- function(tp, sigma0 = sigma0_fitted) {
  t0 <- racefit_start_time(tp)
  df <- damagefunc(tp$racefit_program, sigma0)
  function(t) df(t - t0)
}
slowworkout_damagefunc_tp <- function(tp, sigma0 = sigma0_fitted) {
  t0 <- slow_progressive_start_time(tp)
  df <- damagefunc(tp$slow_workout, sigma0)
  function(t) df(t - t0)
}

# damagefunc for the full programme
# Julia: damagefunc(tp) = t -> rest_dmg*pulse(t,-Inf,0) + sum of all phase damagefuncs
damagefunc.TrainingProgram <- function(tp, sigma0 = sigma0_fitted) {
  rest_dpd    <- tp$rest_program$distance_per_day
  rest_spd    <- tp$rest_program$speed
  df_rest     <- rest_damagefunc(tp, sigma0)
  df_pretrain <- pretraining_damagefunc(tp, sigma0)
  df_slowprog <- slowprogressive_damagefunc(tp, sigma0)
  df_fastprog <- fastprogressive_damagefunc(tp, sigma0)
  df_racefit  <- racefit_damagefunc_tp(tp, sigma0)
  df_slowwrkt <- slowworkout_damagefunc_tp(tp, sigma0)
  function(t) {
    adjusted_damage(rest_dpd, rest_spd, sigma0) * pulse(t, -Inf, 0) +
      df_rest(t)     + df_pretrain(t) +
      df_slowprog(t) + df_fastprog(t) +
      df_racefit(t)  + df_slowwrkt(t)
  }
}

# =============================================================================
# 8.  multiseason()
#     Wraps any phase function to repeat periodically across multiple seasons.
#     Julia: multiseason(tp, f) = t -> f(tp)(rem(t, duration(tp)))
# =============================================================================

multiseason <- function(tp, f) {
  dur    <- duration(tp)
  phase_fn <- f(tp)
  function(t) phase_fn(t %% dur)
}


# =============================================================================
# 9.  ODESystem constructor for a TrainingProgram
#     Builds the mechanical input functions (sigma, strain_rate, damage_rate)
#     for use with solve_bone_model().
#
#     Julia: ModelingToolkit.ODESystem(tp) constructs a symbolic ODE system.
#     In R we simply return a named list of input functions.
# =============================================================================

# build_ode_inputs(): returns a list of functions suitable for solve_bone_model()
# Julia: f_σ(t) = joint_stress(multiseason_speed(t))
#        f_ε(t) = strain_rate_fitted(multiseason_speed(t))
#        f_Dr(t) = multiseason_damage(t)   [damage_input=true mode]
build_ode_inputs <- function(tp) {
  multiseason_speed  <- multiseason(tp, speedfunc)
  multiseason_damage <- multiseason(tp, damagefunc)
  list(
    sigma       = function(t) joint_stress(multiseason_speed(t)),
    strain_rate = function(t) strain_rate_fitted(multiseason_speed(t)),
    # damage_formation_rate replaces n_strides when damage_input = TRUE
    damage_formation_rate = function(t) multiseason_damage(t)
  )
}


# =============================================================================
# 10. construct_training_program()
#     Builds a TrainingProgram with default or custom parameters.
#     Mirrors the Julia function of the same name (lines 166-211).
# =============================================================================

construct_training_program <- function(
  # Rest phase
  rest_duration            = 44,
  rest_distance_per_day    = 4000,
  rest_speed               = 3.6,
  # Pre-training phase
  pretraining_duration          = 4 * 7,
  pretraining_distance_per_day  = 2000,
  pretraining_speed             = 7.5,
  # Slow progressive phase
  slow_progressive_duration               = 4 * 7,
  slow_progressive_distance_per_day_start = 900 / 7,
  slow_progressive_distance_per_day_end   = 1800 / 7,
  slow_progressive_speed_start            = 11.8,
  slow_progressive_speed_end              = 13.8,
  # Fast progressive phase
  fast_progressive_duration            = 38,
  fast_progressive_cumulative_distance1 = 6600,
  fast_progressive_cumulative_distance2 = 3200,
  fast_progressive_speed1              = 13.8,
  fast_progressive_speed2              = 16.0,
  # Race-fit phase
  racefit_duration                     = 8 * 7,
  racefit_slow_gallop_distance_per_day = 4800 / 30,
  racefit_fast_gallop_distance_per_day = 3200 / 30,
  racefit_race_distance_per_day        = 800 / 7,
  racefit_slow_gallop_speed            = 13.8,
  racefit_fast_gallop_speed            = 16.0,
  racefit_race_speed                   = 16.7,
  # Slow workout (canter, runs during progressive + race-fit phases)
  slow_workout_distance_per_day = 2000,
  slow_workout_speed            = 7.5
) {
  rest <- HomogeneousProgram(rest_duration, rest_distance_per_day, rest_speed)

  pretraining <- HomogeneousProgram(pretraining_duration,
                                    pretraining_distance_per_day,
                                    pretraining_speed)

  slow_progressive <- SlowProgressiveProgram(
    slow_progressive_duration,
    slow_progressive_distance_per_day_start,
    slow_progressive_distance_per_day_end,
    slow_progressive_speed_start,
    slow_progressive_speed_end
  )

  fast_progressive <- FastProgressiveProgram(
    fast_progressive_duration,
    fast_progressive_cumulative_distance1,
    fast_progressive_cumulative_distance2,
    fast_progressive_speed1,
    fast_progressive_speed2
  )

  racefit <- RacefitProgram(
    racefit_duration,
    racefit_slow_gallop_distance_per_day,
    racefit_fast_gallop_distance_per_day,
    racefit_race_distance_per_day,
    racefit_slow_gallop_speed,
    racefit_fast_gallop_speed,
    racefit_race_speed
  )

  # Slow workout runs from start of slow progressive through end of race-fit
  slow_workout_duration <- slow_progressive_duration +
                           fast_progressive_duration +
                           racefit_duration
  slow_workout <- HomogeneousProgram(slow_workout_duration,
                                     slow_workout_distance_per_day,
                                     slow_workout_speed)

  TrainingProgram(rest, pretraining, slow_progressive,
                  fast_progressive, racefit, slow_workout)
}
















# =============================================================================
# Extendend program???

# 11. fast_phase_distances()
#     Helper to compute cumulative distances for the fast progressive phase.
#     Julia: fast_phase_distances(total_distance_per_day,
#                                 fast_gallop_distance_per_day_end,
#                                 program_duration)
# =============================================================================

fast_phase_distances <- function(total_distance_per_day,
                                 fast_gallop_distance_per_day_end,
                                 program_duration) {
  total_fast_prog_distance <- total_distance_per_day * program_duration
  # Triangle rule: cumulative distance2 = final_rate * duration / 2
  fast2_prog_distance <- fast_gallop_distance_per_day_end * program_duration / 2
  fast1_prog_distance <- total_fast_prog_distance - fast2_prog_distance
  list(fast1 = fast1_prog_distance, fast2 = fast2_prog_distance)
}


# =============================================================================
# 12. LongFreshenup and short_freshenup
#     Back-off period structures used in ExtendedProgram.
#     Julia: short_freshenup(duration=14) = HomogeneousProgram(duration,1800/7,11.8)
#            LongFreshenup = struct with rest, canter, gallop sub-programs
# =============================================================================

short_freshenup <- function(dur = 14) {
  HomogeneousProgram(dur, 1800 / 7, 11.8)
}

LongFreshenup <- function(program_rest, program_canter, program_gallop) {
  structure(
    list(
      program_rest   = program_rest,
      program_canter = program_canter,
      program_gallop = program_gallop
    ),
    class = c("LongFreshenup", "TrainingPhase")
  )
}

long_freshenup <- function() {
  LongFreshenup(
    program_rest   = HomogeneousProgram(14, 2000, 6),     # trot/canter at 6 m/s
    program_canter = HomogeneousProgram(7,  2000, 7.5),   # canter at 7.5 m/s
    program_gallop = HomogeneousProgram(7,  1800 / 7, 11.8) # slow gallop
  )
}

duration.LongFreshenup <- function(tp) {
  duration(tp$program_rest) + duration(tp$program_canter) + duration(tp$program_gallop)
}

canter_start_time_lf  <- function(tp) duration(tp$program_rest)
gallop_start_time_lf  <- function(tp) canter_start_time_lf(tp) + duration(tp$program_canter)

speedfunc.LongFreshenup <- function(tp) {
  t_cant <- canter_start_time_lf(tp)
  t_gall <- gallop_start_time_lf(tp)
  sf_rest <- speedfunc(tp$program_rest)
  sf_cant <- speedfunc(tp$program_canter)
  sf_gall <- speedfunc(tp$program_gallop)
  function(t) sf_rest(t) + sf_cant(t - t_cant) + sf_gall(t - t_gall)
}

distancefunc.LongFreshenup <- function(tp) {
  t_cant <- canter_start_time_lf(tp)
  t_gall <- gallop_start_time_lf(tp)
  df_rest <- distancefunc(tp$program_rest)
  df_cant <- distancefunc(tp$program_canter)
  df_gall <- distancefunc(tp$program_gallop)
  function(t) df_rest(t) + df_cant(t - t_cant) + df_gall(t - t_gall)
}

damagefunc.LongFreshenup <- function(tp, sigma0 = sigma0_fitted) {
  t_cant  <- canter_start_time_lf(tp)
  t_gall  <- gallop_start_time_lf(tp)
  df_rest <- damagefunc(tp$program_rest, sigma0)
  df_cant <- damagefunc(tp$program_canter, sigma0)
  df_gall <- damagefunc(tp$program_gallop, sigma0)
  function(t) df_rest(t) + df_cant(t - t_cant) + df_gall(t - t_gall)
}


# =============================================================================
# 13. ExtendedProgram
#     Extends TrainingProgram with interspersed back-off (freshenup) periods
#     between race-fit campaigns.
#     Julia struct: ExtendedProgram(rest, pretraining, slow_prog, fast_prog,
#                                   racefit, freshenup, slow_workout,
#                                   num_freshenups, slow_workout_pause)
# =============================================================================

ExtendedProgram <- function(rest_program,
                            pretraining_program,
                            slow_progressive_program,
                            fast_progressive_program,
                            racefit_program,
                            freshenup_program,
                            slow_workout,
                            num_freshenups,
                            slow_workout_pause) {
  structure(
    list(
      rest_program             = rest_program,
      pretraining_program      = pretraining_program,
      slow_progressive_program = slow_progressive_program,
      fast_progressive_program = fast_progressive_program,
      racefit_program          = racefit_program,
      freshenup_program        = freshenup_program,
      slow_workout             = slow_workout,
      num_freshenups           = num_freshenups,
      slow_workout_pause       = slow_workout_pause
    ),
    class = c("ExtendedProgram", "AbstractRepeatedProgram")
  )
}

duration.ExtendedProgram <- function(tp) {
  duration(tp$rest_program) +
    duration(tp$pretraining_program) +
    progressive_program_duration(tp) +
    (tp$num_freshenups + 1) * duration(tp$racefit_program) +
    tp$num_freshenups * duration(tp$freshenup_program)
}

# Start time of the first freshenup (after the first race-fit segment)
freshenup_start_time <- function(tp) {
  racefit_start_time(tp) + duration(tp$racefit_program)
}

# Duration of one race-fit + freshenup cycle
race_cycle_duration <- function(tp) {
  duration(tp$racefit_program) + duration(tp$freshenup_program)
}

# speedfunc for one race cycle (freshenup first, then race-fit)
# Julia: race_cycle_speed_func(tp) = t -> speedfunc(freshenup)(t) +
#                                         speedfunc(racefit)(t - duration(freshenup))
race_cycle_speed_func <- function(tp) {
  dur_fu  <- duration(tp$freshenup_program)
  sf_fu   <- speedfunc(tp$freshenup_program)
  sf_rf   <- speedfunc(tp$racefit_program)
  function(t) sf_fu(t) + sf_rf(t - dur_fu)
}

# speedfunc for the full ExtendedProgram
# Julia: speedfunc(tp::ExtendedProgram) = t -> rest_speed*pulse(t,-Inf,0) +
#   sum of phase speedfuncs + sum over freshenup cycles
speedfunc.ExtendedProgram <- function(tp) {
  t_pretrain  <- pretraining_start_time(tp)
  t_slowprog  <- slow_progressive_start_time(tp)
  t_fastprog  <- fast_progressive_start_time(tp)
  t_racefit   <- racefit_start_time(tp)
  t_fu        <- freshenup_start_time(tp)
  rc_dur      <- race_cycle_duration(tp)
  n           <- tp$num_freshenups
  rest_spd    <- tp$rest_program$speed
  sf_rest     <- speedfunc(tp$rest_program)
  sf_pretrain <- speedfunc(tp$pretraining_program)
  sf_slowprog <- speedfunc(tp$slow_progressive_program)
  sf_fastprog <- speedfunc(tp$fast_progressive_program)
  sf_racefit  <- speedfunc(tp$racefit_program)
  rc_sf       <- race_cycle_speed_func(tp)
  function(t) {
    cycle_sum <- sum(sapply(0:(n - 1), function(i) rc_sf(t - t_fu - i * rc_dur)))
    rest_spd * pulse(t, -Inf, 0) +
      sf_rest(t) +
      sf_pretrain(t - t_pretrain) +
      sf_slowprog(t - t_slowprog) +
      sf_fastprog(t - t_fastprog) +
      sf_racefit(t  - t_racefit) +
      cycle_sum
  }
}

# slowworkout_distancefunc for ExtendedProgram:
# paused during the first slow_workout_pause days of each freshenup cycle
# Julia: slowworkout_distancefunc(tp::ExtendedProgram) = t ->
#   distancefunc(slow_workout)(t - slow_prog_start) *
#   prod(1 - pulse(t, fu_start + i*rc_dur, fu_start + i*rc_dur + pause) for i in 0:n-1)
slowworkout_distancefunc.ExtendedProgram <- function(tp) {
  t0     <- slow_progressive_start_time(tp)
  t_fu   <- freshenup_start_time(tp)
  rc_dur <- race_cycle_duration(tp)
  pause  <- tp$slow_workout_pause
  n      <- tp$num_freshenups
  df_sw  <- distancefunc(tp$slow_workout)
  function(t) {
    pause_factor <- prod(sapply(0:(n - 1), function(i)
      1 - pulse(t, t_fu + i * rc_dur, t_fu + i * rc_dur + pause)
    ))
    df_sw(t - t0) * pause_factor
  }
}

# distancefunc for one race cycle
race_cycle_distancefunc <- function(tp) {
  dur_fu <- duration(tp$freshenup_program)
  df_fu  <- distancefunc(tp$freshenup_program)
  df_rf  <- distancefunc(tp$racefit_program)
  function(t) df_fu(t) + df_rf(t - dur_fu)
}

# distancefunc for the full ExtendedProgram
distancefunc.ExtendedProgram <- function(tp) {
  rest_dpd    <- tp$rest_program$distance_per_day
  t_fu        <- freshenup_start_time(tp)
  rc_dur      <- race_cycle_duration(tp)
  n           <- tp$num_freshenups
  df_rest     <- rest_distancefunc(tp)
  df_pretrain <- pretraining_distancefunc(tp)
  df_slowprog <- slowprogressive_distancefunc(tp)
  df_fastprog <- fastprogressive_distancefunc(tp)
  df_racefit  <- racefit_distancefunc(tp)
  df_slowwrkt <- slowworkout_distancefunc.ExtendedProgram(tp)
  rc_df       <- race_cycle_distancefunc(tp)
  function(t) {
    cycle_sum <- sum(sapply(0:(n - 1), function(i) rc_df(t - t_fu - i * rc_dur)))
    rest_dpd * pulse(t, -Inf, 0) +
      df_rest(t) + df_pretrain(t) + df_slowprog(t) +
      df_fastprog(t) + df_racefit(t) + df_slowwrkt(t) +
      cycle_sum
  }
}

# damagefunc for ExtendedProgram
damagefunc.ExtendedProgram <- function(tp, sigma0 = sigma0_fitted) {
  rest_dpd  <- tp$rest_program$distance_per_day
  rest_spd  <- tp$rest_program$speed
  t_fu      <- freshenup_start_time(tp)
  rc_dur    <- race_cycle_duration(tp)
  n         <- tp$num_freshenups
  dur_fu    <- duration(tp$freshenup_program)
  df_fu     <- damagefunc(tp$freshenup_program, sigma0)
  df_rf_seg <- damagefunc(tp$racefit_program, sigma0)
  rc_dmg    <- function(t) df_fu(t) + df_rf_seg(t - dur_fu)

  # Slow workout damage with pause
  t0_sw  <- slow_progressive_start_time(tp)
  pause  <- tp$slow_workout_pause
  df_sw  <- damagefunc(tp$slow_workout, sigma0)
  sw_dmg <- function(t) {
    pause_factor <- prod(sapply(0:(n - 1), function(i)
      1 - pulse(t, t_fu + i * rc_dur, t_fu + i * rc_dur + pause)
    ))
    df_sw(t - t0_sw) * pause_factor
  }

  df_rest     <- function(t) damagefunc(tp$rest_program, sigma0)(t - rest_start_time(tp))
  df_pretrain <- function(t) damagefunc(tp$pretraining_program, sigma0)(t - pretraining_start_time(tp))
  df_slowprog <- function(t) damagefunc(tp$slow_progressive_program, sigma0)(t - slow_progressive_start_time(tp))
  df_fastprog <- function(t) damagefunc(tp$fast_progressive_program, sigma0)(t - fast_progressive_start_time(tp))
  df_racefit  <- function(t) damagefunc(tp$racefit_program, sigma0)(t - racefit_start_time(tp))

  function(t) {
    cycle_sum <- sum(sapply(0:(n - 1), function(i) rc_dmg(t - t_fu - i * rc_dur)))
    adjusted_damage(rest_dpd, rest_spd, sigma0) * pulse(t, -Inf, 0) +
      df_rest(t) + df_pretrain(t) + df_slowprog(t) +
      df_fastprog(t) + df_racefit(t) + sw_dmg(t) +
      cycle_sum
  }
}


# =============================================================================
# 14. construct_extended_training_program()
#     Mirrors the Julia function of the same name (lines 293-339).
# =============================================================================

construct_extended_training_program <- function(
  rest_duration            = 6 * 7,
  rest_distance_per_day    = 4000,
  rest_speed               = 3.6,
  pretraining_duration          = 4 * 7,
  pretraining_distance_per_day  = 2000,
  pretraining_speed             = 7.5,
  progressive_duration          = 10 * 7,
  slow_progressive_distance_per_day_start = 900 / 7,
  slow_progressive_distance_per_day_end   = 1800 / 7,
  slow_progressive_speed_start = 11.8,
  slow_progressive_speed_end   = 13.8,
  total_distance_per_day       = 1805 / 7,
  distance2_per_day_end        = 1179 / 7,
  fast_progressive_speed1      = 13.8,
  fast_progressive_speed2      = 16.0,
  racefit_duration                     = 4.5 * 7,
  racefit_slow_gallop_distance_per_day = 4800 / 30,
  racefit_fast_gallop_distance_per_day = 3200 / 30,
  racefit_race_distance_per_day        = 800 / 7,
  racefit_slow_gallop_speed            = 13.8,
  racefit_fast_gallop_speed            = 16.0,
  racefit_race_speed                   = 16.7,
  freshenup_program        = long_freshenup(),
  num_freshenups           = 3,
  slow_workout_distance_per_day = 2000,
  slow_workout_speed            = 7.5,
  slow_workout_pause            = 14
) {
  rest        <- HomogeneousProgram(rest_duration, rest_distance_per_day, rest_speed)
  pretraining <- HomogeneousProgram(pretraining_duration, pretraining_distance_per_day, pretraining_speed)

  slow_progressive_duration <- 0.4 * progressive_duration
  slow_progressive <- SlowProgressiveProgram(
    slow_progressive_duration,
    slow_progressive_distance_per_day_start,
    slow_progressive_distance_per_day_end,
    slow_progressive_speed_start,
    slow_progressive_speed_end
  )

  fast_progressive_duration <- 0.6 * progressive_duration
  dists <- fast_phase_distances(total_distance_per_day,
                                distance2_per_day_end,
                                fast_progressive_duration)
  fast_progressive <- FastProgressiveProgram(
    fast_progressive_duration,
    dists$fast1,
    dists$fast2,
    fast_progressive_speed1,
    fast_progressive_speed2
  )

  racefit <- RacefitProgram(
    racefit_duration,
    racefit_slow_gallop_distance_per_day,
    racefit_fast_gallop_distance_per_day,
    racefit_race_distance_per_day,
    racefit_slow_gallop_speed,
    racefit_fast_gallop_speed,
    racefit_race_speed
  )

  slow_workout_duration <- slow_progressive_duration + fast_progressive_duration +
    racefit_duration + num_freshenups * (racefit_duration + duration(freshenup_program))
  slow_workout <- HomogeneousProgram(slow_workout_duration,
                                     slow_workout_distance_per_day,
                                     slow_workout_speed)

  ExtendedProgram(rest, pretraining, slow_progressive, fast_progressive,
                  racefit, freshenup_program, slow_workout,
                  num_freshenups, slow_workout_pause)
}
