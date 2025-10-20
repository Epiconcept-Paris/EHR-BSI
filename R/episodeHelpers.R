# ----------------------------
# Function that assigns episode id within one patient
# ----------------------------
assign_episodes <- function(df_one_pt, episodeDuration) {
  ep_id      <- 0L
  active_yet <- FALSE
  
  epi_vec    <- integer(nrow(df_one_pt))
  start_vec  <- as.Date(rep(NA, nrow(df_one_pt)))
  
  to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
    if (is.character(x)) {
      xs <- trimws(x)
      num_idx <- suppressWarnings(!is.na(as.numeric(xs)))
      out <- rep(as.Date(NA), length(xs))
      if (any(num_idx)) {
        out[num_idx] <- as.Date(as.numeric(xs[num_idx]), origin = "1899-12-30")
      }
      try_formats <- c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
        "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
        "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
        "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%d.%m.%Y",
        "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y/%m/%d"
      )
      need_parse <- which(is.na(out))
      if (length(need_parse) > 0) {
        parsed <- suppressWarnings(try(as.POSIXlt(xs[need_parse], tz = "", tryFormats = try_formats), silent = TRUE))
        if (!inherits(parsed, "try-error")) {
          out[need_parse] <- as.Date(parsed)
        }
      }
      return(out)
    }
    parsed <- suppressWarnings(try(as.POSIXlt(x, tz = "", tryFormats = c("%Y-%m-%d", "%d/%m/%Y")), silent = TRUE))
    if (inherits(parsed, "try-error")) return(as.Date(NA))
    as.Date(parsed)
  }
  
  for (i in seq_len(nrow(df_one_pt))) {
    cur_date <- to_date(df_one_pt$OnsetDate[i])
    cur_org  <- df_one_pt$MicroorganismCode[i]
    
    if (!active_yet) {
      ep_id      <- ep_id + 1L
      active_yet <- TRUE
      epi_start  <- cur_date
      epi_orgs   <- cur_org
    } else {
      gap_days <- as.integer(cur_date - epi_start)
      same_org <- cur_org %in% epi_orgs
      
      need_new <- case_when(
        same_org  & gap_days <= as.numeric(episodeDuration-1) ~ FALSE,
        !same_org & gap_days <= 2L  ~ FALSE,
        TRUE                        ~ TRUE
      )
      
      if (need_new) {
        ep_id     <- ep_id + 1L
        epi_start <- cur_date
        epi_orgs  <- cur_org
      } else {
        epi_orgs  <- union(epi_orgs, cur_org)
      }
    }
    epi_vec[i]   <- ep_id
    start_vec[i] <- epi_start
  }
  
  df_one_pt %>%
    mutate(EpisodeNumber    = epi_vec,
           EpisodeStartDate = start_vec)
}


## helper ─ find all ≥2-positive CC windows inside the admission
flag_cc_clusters <- function(dates, episode_duration) {
  dates <- sort(as.Date(dates[!is.na(dates)]))   # keep only real dates
  n     <- length(dates)
  out   <- logical(n)
  
  i <- 1L
  while (i < n) {
    
    if (dates[i + 1] - dates[i] <= 2) {          # 2nd culture within 3 days?
      out[i] <- TRUE                             # → mark start of the cluster
      
      ## jump to first sample that is *outside* the 14-day episode window
      nxt <- which(dates > dates[i] + episode_duration -1 )
      if (length(nxt) == 0) break                # none found → we are done
      i <- nxt[1]
    } else {
      i <- i + 1L
    }
  }
  out
}
