#CE-QUAL-W2 temperature modeling of Willamette Reservoirs"

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(w2r)
library(foreach)
library(furrr)
library(processx)
library(future.callr)
plan(callr)


c2f = function(x) {
  (x * 9 / 5) + 32
}
f2c = function(x) {
  (x - 32) * 5 / 9
}

# Set working directory
wd = "C:/Users/g2echnb9/Documents/WillametteDisposition2023"
DataDir = file.path(wd, "RES-SIM")
projFldr = "DcmPnstck"
# Where to get RES-SIM data
RefDataDir = file.path(wd, "RefData")
verDir = ""
scenarios = data.frame(Baseline = NA, DcmPnstck = NA, RcnPnstck = NA, StationService = NA)

years = c('2011','2015','2016')
# Read in RES-SIM to W2 codes
RessimCodes = read_csv(file.path(DataDir, "WillW2OutputNmsSeg.csv"))
# Load outlets at each dam
source(file.path(DataDir, "QGT_elvsFrom_w2_con_2021-11-01.r"))

# Load the Rule Curves
source(file.path(DataDir, "WVWQfunctions.r"))

RuleCurveDir = file.path(RefDataDir, "rule_curves")
rcs = ReadRuleCurves(RuleCurveDir) |> 
   expand_grid(tibble(Year = factor(c(2011, 2015, 2016))))  |>
   expand_grid(tibble(Alt = names(scenarios))) |>
   mutate(Site = factor(Site))

elvVolCurvPath = file.path(RefDataDir, "elv_vol_curves")

source(file.path(DataDir,'watBalIter.r'))
wbSummary <- watBalIter()

# run headwaters first
wbSelect = wbSummary |>
  filter(
    #Scenario != 'StnService',
    MAE > 0.5,
    Flag
  ) |>
  select(RESSIMCode, Scenario, Year,wb)


while(nrow(wbSelect)>0){
  
  # now actually make the change
  wbSummary |>
    inner_join(wbSelect) |>
    distinct(path, seg = SegmentOutput, wb, write.files = Flag,
        elvVolCrvFl, RESSIMCode = ReachName,maxElvDif_m = maxElvDif_m) |>
    mutate(Result = pmap(
      list(path = path, seg = seg, wb = wb, write.files = write.files,
        elvVolCrvFl = elvVolCrvFl, RESSIMCode = RESSIMCode,maxElvDif_m = maxElvDif_m),
      function(...)
        readW2ConInOut(..., wd = wd)$watbal$fit
    ))
  
  # run the model
  runs = wbSummary |>
    inner_join(wbSelect) |>
    # sort by run order
    arrange(W2name) |>
    distinct(path) |>
    mutate(process = map(path, function(x) {
      Sys.sleep(2)
      process$new("w2_4p2_gen64opt1_usgs7.exe", wd = x,
        supervise = TRUE, stdout = "|", stderr = "|")
    }))
  
  print(paste('Running',nrow(runs),'models...'))
  Sys.sleep(30)
  
  # check running
  modStatus <- map_lgl(runs$process, function(x) x$is_alive())
  while(any(modStatus)){
    Sys.sleep(60*2)
    modStatus <- map_lgl(runs$process, function(x) x$is_alive())
  }
  # Clean up
  rm(wbSummary,wbSelect,modStatus,runs)  
  # 
  # tail(runs$process[[1]]$read_output_lines()) #fail
  # tail(runs$process[[2]]$read_output_lines()) # fail
  # tail(runs$process[[3]]$read_output_lines()) #fail
  # tail(runs$process[[4]]$read_output_lines()) # fail
  # tail(runs$process[[5]]$read_output_lines()) # fail
  #tail(runs$process[[6]]$read_output_lines()) # fail
  #
  #runs$process[[1]]$get_exit_status() #fail
  #runs$process[[2]]$get_exit_status() # fail
  #runs$process[[3]]$get_exit_status() #fail
  #runs$process[[4]]$get_exit_status() # fail
  #runs$process[[5]]$get_exit_status()
  wbSummary <- watBalIter()
  
  # plt <- ggplot(wbSummary |> filter(
  #   #Flag
  # ) ) +
  #   aes(x = Year, y = MAE, fill = Flag) +
  #   geom_hline(yintercept= 0.2, color = "red") +
  #   geom_point(size = 5, shape = 21) +
  #   ggtitle('Water Balance') +
  #   facet_grid(c("W2name", "Scenario")) +
  #   scale_x_discrete(NULL) +
  #   scale_y_log10() +
  #   scale_fill_manual("Modification Allowed", values = c("TRUE" = "lightblue", "FALSE" = "grey70")) +
  #   theme_bw(18) +
  #   theme(legend.position = "bottom")
  # 
  # ggsave(plt, file.path(wd, projFldr, verDir, "WaterBalanceSummary.png"),
  #        width = 9,height = 6)
  # 
  
  # run headwaters first
  wbSelect = wbSummary |>
    filter(
      #Scenario != 'StnService',
      MAE > 0.5,
      Flag
    ) |>
    select(RESSIMCode, Scenario, Year,wb) #,MAE

} # End while loop




# reset QDT
reruns = wbMod |>
  inner_join(wbSelect) |>
  mutate(Modified = pmap(
    list(path = path, qdt = "OFF", seg = SegmentOutput, wb = wb, Flag = Flag),
    function(..., Flag) {
      if (Flag) {
        modifyW2con(...)
        TRUE
      } else {
        FALSE
      }
    }
  )) |>
  arrange(W2name) |>
  pull(path) |>
  unique() |>
  # name processes
  set_names(basename) |>
  map(function(x) {
    Sys.sleep(2)
    process$new("w2_4p2_gen64opt1_usgs7.exe", wd = x,
      supervise = TRUE, stdout = "|", stderr = "|")
  })


# and turn QDT back on
wbMod |>
  inner_join(wbSelect) |>
  mutate(Modified = pmap(
    list(path = path, qdt = "ON", seg = SegmentOutput, wb = wb, Flag = Flag),
    function(..., Flag) {
      if (Flag) {
        modifyW2con(...)
        TRUE
      } else {
        FALSE
      }
    }
  ))
```
