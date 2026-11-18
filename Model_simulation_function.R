source("bone_adaptation_model.R")
source("detailed_training_program.R")
library(dplyr)


simulate_program <- function(tp, n_seasons = n_seasons, params = sim_params) {
  
  # Initial condition: steady-state fBM at trot speed
  
  fBM0 <- initial_fBM(params)
  
  inputs <- build_ode_inputs(tp)  #additional imputs from training component
  
  # Total simulation duration
  prog_dur <- duration(tp)
  t_end    <- n_seasons * prog_dur
  
  solve_bone_model(
    inputs  = inputs,
    y0      = c(fBM0, 0),
    t_end   = t_end,
    params  = params
  )
}

training_periods <- function(tp, n_seasons) {
  prog_dur    <- duration(tp)
  t_rest      <- pretraining_start_time(tp)
  t_pretrain  <- slow_progressive_start_time(tp)
  t_prog      <- racefit_start_time(tp)
  
  lapply(seq_len(n_seasons), function(j) {
    offset <- (j - 1) * prog_dur
    list(
      start        = offset,
      rest_end     = offset + t_rest,
      pretrain_end = offset + t_pretrain,
      prog_end     = offset + t_prog,
      end          = offset + prog_dur
    )
  })
}

summarise_phase_changes <- function(sol, tp, n_seasons) {
  seasons <- training_periods(tp, n_seasons)
  
  bind_rows(lapply(seq_len(n_seasons), function(j) {
    times <- seasons[[j]]
    t_start = (j-1)*duration(tp)
    t_end = j*duration(tp)
    val <- function(t) {
      list(
        fBM = approx(sol$t, sol$fBM, xout = t)$y,
        dmg = approx(sol$t, sol$dmg, xout = t)$y
      )
    }
    
    v_start   <- val(times$start)
    v_rest    <- val(times$rest_end)
    v_pretrain <- val(times$pretrain_end)
    v_prog    <- val(times$prog_end)
    v_end     <- val(times$end)
    # cat("j=",j)
    
    t_rest_phase <- seq(t_start, t_start + pretraining_start_time(tp), by = 0.1)
    # fBM_rest <- approx(sol$t, sol$fBM, xout = t_rest_phase)$y
    dmg_rest <- approx(sol$t, sol$dmg, xout = t_rest_phase)$y
    dmg_min_rest<-min(dmg_rest)
    dmg_max_rest<-max(dmg_rest)
    
    t_pretrain_phase <- seq(t_start+pretraining_start_time(tp),t_start+slow_progressive_start_time(tp) , by = 0.1)
    # fBM_pretrain <- approx(sol$t, sol$fBM, xout = t_pretrain_phase)$y
    dmg_pretrain <- approx(sol$t, sol$dmg, xout = t_pretrain_phase)$y
    dmg_min_pretrain<-min(dmg_pretrain)
    dmg_max_pretrain<-max(dmg_pretrain)
    
    t_prog_phase <- seq(t_start+slow_progressive_start_time(tp),t_start+racefit_start_time(tp) , by = 0.1)
    # fBM_pretrain <- approx(sol$t, sol$fBM, xout = t_pretrain_phase)$y
    dmg_prog <- approx(sol$t, sol$dmg, xout = t_prog_phase)$y
    dmg_min_prog<-min(dmg_prog)
    dmg_max_prog<-max(dmg_prog)
    
    t_race_phase <- seq(t_start+racefit_start_time(tp),t_end, by = 0.1)
    # fBM_pretrain <- approx(sol$t, sol$fBM, xout = t_pretrain_phase)$y
    dmg_race <- approx(sol$t, sol$dmg, xout = t_race_phase)$y
    dmg_min_race<-min(dmg_race)
    dmg_max_race<-max(dmg_race)
 
    
    t_all_phase <- seq(t_start,t_end, by = 0.1)
    # fBM_pretrain <- approx(sol$t, sol$fBM, xout = t_pretrain_phase)$y
    dmg_all <- approx(sol$t, sol$dmg, xout = t_all_phase)$y
    dmg_min_all<-min(dmg_all)
    dmg_max_all<-max(dmg_all)   
    
    data.frame(
      preparation = j,
      phase = c(1,2,3,4,5),
      dmg_min = c(dmg_min_rest,dmg_min_pretrain,dmg_min_prog,dmg_min_race,dmg_min_all),
      dmg_max = c(dmg_max_rest,dmg_max_pretrain,dmg_max_prog,dmg_max_race,dmg_max_all),
      fBM_change = c(
        v_rest$fBM    - v_start$fBM,
        v_pretrain$fBM - v_rest$fBM,
        v_prog$fBM    - v_pretrain$fBM,
        v_end$fBM     - v_prog$fBM,
        v_end$fBM     - v_start$fBM
      ),
      dmg_change = c(
        v_rest$dmg    - v_start$dmg,
        v_pretrain$dmg - v_rest$dmg,
        v_prog$dmg    - v_pretrain$dmg,
        v_end$dmg     - v_prog$dmg,
        v_end$dmg     - v_start$dmg
      ),
      start_time=c(times$start,times$rest_end,times$pretrain_end,times$prog_end,t_start),
      end_time  =c(times$rest_end,times$pretrain_end,times$prog_end,times$end,t_end),
      my_color = c("#99daff", "#C1E9DE","#ffe099", "#e3b5ce","#80b1d3")
    )
  }))
}

Trainig_model_R<-function(program_duration_UI,program_volume_UI,program_speed_UI,n_seasons){
  cat("n_seasons=",n_seasons)
  rest_dur =program_duration_UI[1]
  pre_dur = program_duration_UI[2]
  prog_dur = program_duration_UI[3]
  race_dur = program_duration_UI[4]
  
  rest_dist_pr_day = program_volume_UI[1]
  pre_dist_pr_day = program_volume_UI[2]
  slow_progressive_distance_per_day_start = program_volume_UI[3]
  slow_progressive_distance_per_day_end = program_volume_UI[4]
  fast_progressive_speed1 = program_volume_UI[5]
  fast_progressive_speed2 = program_volume_UI[6]
  
  racefit_slow_gallop_distance_per_day=program_volume_UI[7]
  racefit_fast_gallop_distance_per_day=program_volume_UI[8]
  racefit_race_distance_per_day = program_volume_UI[9]
  
  slow_workout_distance_per_day = program_volume_UI[10]
  
  
  
  rest_speed = program_speed_UI[1]
  pretraining_speed = program_speed_UI[2]
  slow_progressive_speed_start = program_speed_UI[3]
  slow_progressive_speed_end = program_speed_UI[4]
  fast_progressive_speed1 = program_speed_UI[5]
  fast_progressive_speed2 = program_speed_UI[6]
  racefit_slow_gallop_speed = program_speed_UI[7]
  racefit_fast_gallop_speed = program_speed_UI[8]
  racefit_race_speed = program_speed_UI[9]
  
  slow_workout_speed = program_speed_UI[10]
    
  tp <- construct_training_program(
    rest_duration = rest_dur,
    pretraining_duration = pre_dur,
    slow_progressive_duration = 0.4*prog_dur,
    fast_progressive_duration = 0.6*prog_dur,
    racefit_duration = race_dur,
    # 
    rest_distance_per_day = rest_dist_pr_day,
    pretraining_distance_per_day = pre_dist_pr_day,
    slow_progressive_distance_per_day_start = slow_progressive_distance_per_day_start,
    slow_progressive_distance_per_day_end = slow_progressive_distance_per_day_end,
    # racefit_slow_gallop_distance_per_day=4800/30, racefit_fast_gallop_distance_per_day=3200/30, racefit_race_distance_per_day=800/7,
    racefit_slow_gallop_distance_per_day=racefit_slow_gallop_distance_per_day,
    racefit_fast_gallop_distance_per_day= racefit_fast_gallop_distance_per_day,
    racefit_race_distance_per_day = racefit_race_distance_per_day,
    
    
    slow_workout_distance_per_day = slow_workout_distance_per_day,
    
    # 
    rest_speed=rest_speed,
    pretraining_speed=pretraining_speed,
    slow_progressive_speed_start=slow_progressive_speed_start,
    slow_progressive_speed_end=slow_progressive_speed_end,
    fast_progressive_speed1=fast_progressive_speed1,
    fast_progressive_speed2=fast_progressive_speed2,
    racefit_slow_gallop_speed = racefit_slow_gallop_speed,
    racefit_fast_gallop_speed = racefit_fast_gallop_speed,
    racefit_race_speed = racefit_race_speed,
    
    
    slow_workout_speed = slow_workout_speed
  )
  
  sols_R<-simulate_program(tp,n_seasons=n_seasons) 
  tp_ave <- construct_training_program()
  sols_R_ave <- simulate_program(tp_ave,n_seasons=n_seasons)   
  df_changes<-summarise_phase_changes(sols_R, tp, n_seasons)
  return(list(sols_R=sols_R, df_changes=df_changes, sols_R_ave=sols_R_ave))
}
