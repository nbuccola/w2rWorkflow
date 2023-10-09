watBalIter <- function(){
  # Water Balance steps:
  # 1. Run the W2 models a first time with QDT set to OFF. 
  # 2. Run the following code chunk to calculate the missing flow needed (QDT) to meet the operational lake levels and close the water balance for each reservoir. Use the WillW2_HCR-LOP_IRRM_2020_watbalSwitch.csv file to control which reservoir, year, and scenario needs a new QDT (re-)written. 
  # 3. Re-Run W2 model with QDT set to ON and repeat step 2. 
  # 4. Repeat steps 2 and 3 as needed. In the MF Willamette, only one iteration was needed.
  
  # RunWaterBalance, echo = FALSE, include = FALSE, results = "hide"
  wbFits = read_csv(file.path(wd, "WillW2_HydDisp_watbalSwitch.csv")) |>
    mutate(Year = factor(round(Year))) |>
    # turn wide table into long table
    pivot_longer(!all_of(c("RESSIMCode", "Year","maxElvDif_m")),
                 names_to = "Scenario", values_to = "Flag") |>
    # attach WQ model codes
    left_join(RessimCodes, by = "RESSIMCode") |>
    mutate(
      W2name = factor(W2name, c("hcr", "mfwill", "lopdex", "cgr", "gpfos", "det", "bcl")),
      # get working directory for each model run
      path = file.path(wd, Scenario, W2name, paste0(W2name, Year)),
      pathExists = file.exists(path),
      # identify qdt value for every run
      qdt = if_else(Flag, "ON", "OFF"),
      # get path to elevation-volume curve for every run
      # should error if you get more than 1 curve per model
      elvVolCrvFl = map_chr(RESSIMCode, function(x)
        c(list.files(elvVolCurvPath, full.names = TRUE,
                     pattern = sprintf("^%s_elev_curve_.*\\.csv$", x)),
          NA_character_)[1]
      )
    )
  
  # walk through model
  wbMod = wbFits |>
    # drop rows that we are not investigating
    filter(!is.na(Flag)) |>
    mutate(Modified = pmap(
      list(path = path, qdt = qdt, seg = SegmentOutput, wb = wb, Flag = Flag),
      function(..., Flag) {
        if (Flag) {
          modifyW2con(...)
          TRUE
        } else {
          FALSE
        }
      }
    ))
  
  
  
  
  # always run check first
  wbSummary = wbMod |>
    filter(
      #W2name == "lopdex"
    ) |>
    mutate(Result = pmap(
      list(path = path, seg = SegmentOutput, wb = wb, write.files = FALSE,
           elvVolCrvFl = elvVolCrvFl, RESSIMCode = ReachName,maxElvDif_m = maxElvDif_m),
      function(...)
        readW2ConInOut(..., wd = wd)$watbal$fit
    )) |>
    hoist(Result, "N", "ME", "MAE") |>
    mutate(Flag = MAE >0.5)
  
   
  return(wbSummary)
}