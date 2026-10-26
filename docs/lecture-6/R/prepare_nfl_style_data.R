# Run from the Lecture 6 folder: Rscript R/prepare_nfl_style_data.R
# Downloads are only needed if the local source files are absent.
options(timeout = 600)
seasons <- 2022:2024
raw_dir <- "data/nfl-style-raw"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
features <- c("dropback_rate","early_down_dropback_rate","shotgun_rate",
              "no_huddle_rate","mean_air_yards","deep_target_rate",
              "middle_target_rate","outside_run_rate")
rows <- list(); audits <- list(); provenance <- list()
for (s in seasons) {
  url <- sprintf("https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_%d.rds",s)
  path <- file.path(raw_dir,basename(url))
  if (!file.exists(path)) download.file(url,path,mode="wb")
  p <- as.data.frame(readRDS(path))
  needed <- c("season","season_type","posteam","game_id","play_id","qtr",
              "score_differential","half_seconds_remaining","play_type","down",
              "qb_kneel","qb_spike","two_point_attempt","penalty",
              "qb_dropback","shotgun","no_huddle","air_yards",
              "pass_location","run_location","qb_scramble")
  stopifnot(all(needed %in% names(p)), !anyDuplicated(paste(p$game_id,p$play_id)))
  p <- p[p$season==s & p$season_type=="REG",needed]
  regular_n <- nrow(p)
  valid <- with(p, !is.na(posteam) & nzchar(posteam) &
    play_type %in% c("pass","run") & qtr %in% 1:3 &
    is.finite(score_differential) & abs(score_differential)<=7 &
    is.finite(half_seconds_remaining) & half_seconds_remaining>120 &
    !is.na(penalty) & penalty==0 &
    !is.na(qb_kneel) & qb_kneel==0 &
    !is.na(qb_spike) & qb_spike==0 &
    !is.na(two_point_attempt) & two_point_attempt==0)
  p <- p[which(valid),]
  stopifnot(length(unique(p$posteam))==32)
  for (team in sort(unique(p$posteam))) {
    z <- p[p$posteam==team,]
    early <- z$down %in% c(1,2)
    target <- z$play_type=="pass" & is.finite(z$air_yards)
    location <- target & z$pass_location %in% c("left","middle","right")
    runs <- z$play_type=="run" & !is.na(z$qb_scramble) & z$qb_scramble==0 &
      z$run_location %in% c("left","middle","right")
    avg <- function(v) mean(v,na.rm=TRUE)
    rows[[length(rows)+1]] <- data.frame(
      season=s,team=team,label=paste(team,s,sep="-"),n_plays=nrow(z),
      n_early=sum(early),n_targets=sum(target),n_target_locations=sum(location),
      n_located_runs=sum(runs),
      n_dropback_known=sum(!is.na(z$qb_dropback)),
      n_early_dropback_known=sum(!is.na(z$qb_dropback[early])),
      n_shotgun_known=sum(!is.na(z$shotgun)),n_no_huddle_known=sum(!is.na(z$no_huddle)),
      dropback_rate=avg(z$qb_dropback),
      early_down_dropback_rate=avg(z$qb_dropback[early]),
      shotgun_rate=avg(z$shotgun),no_huddle_rate=avg(z$no_huddle),
      mean_air_yards=avg(z$air_yards[target]),
      deep_target_rate=avg(z$air_yards[target]>=20),
      middle_target_rate=avg(z$pass_location[location]=="middle"),
      outside_run_rate=avg(z$run_location[runs] %in% c("left","right")))
  }
  audits[[as.character(s)]] <- data.frame(season=s,regular_pbp_rows=regular_n,retained_plays=nrow(p))
  provenance[[as.character(s)]] <- data.frame(season=s,url=url,md5=unname(tools::md5sum(path)))
}
profiles <- do.call(rbind,rows)
audit <- do.call(rbind,audits)
stopifnot(nrow(profiles)==96,!anyDuplicated(profiles$label),
          !anyNA(profiles[features]),all(is.finite(as.matrix(profiles[features]))),
          all(vapply(profiles[features],sd,numeric(1))>0))
rates <- setdiff(features,"mean_air_yards")
stopifnot(all(as.matrix(profiles[rates])>=0),all(as.matrix(profiles[rates])<=1))
write.csv(profiles,"data/nfl_offensive_styles_2022_2024.csv",row.names=FALSE)
write.csv(audit,"data/nfl_style_filter_audit.csv",row.names=FALSE)
write.csv(do.call(rbind,provenance),"data/nfl_style_sources.csv",row.names=FALSE)
saveRDS(list(profiles=profiles,features=features,audit=audit,
  provenance=do.call(rbind,provenance),retrieved="2026-10-25"),
  "data/nfl_offensive_styles_2022_2024.rds")
print(audit)
print(summary(profiles[c("n_plays","n_early","n_targets","n_target_locations","n_located_runs")]))

