#' @import airGR
#' @export
create_GR4J <- function(x, ...) {
  UseMethod("create_GR4J", x)
}

#' create_GR4J
#' @param d A data.table with the columns of `date`, `P`, `E`, `Robs`
#' 
#' @rdname create_GR4J
#' @export
create_GR4J.data.frame <- function(
    d, inds_calib = NULL, inds_valid = NULL, perc_calib = 0.7, ...) {

  n = nrow(d)
  ncalib = ceiling(n*perc_calib)
  if (is.null(inds_calib)) inds_calib = 1:ncalib
  if (is.null(inds_valid)) inds_valid = (ncalib+1):n

  date = d$date
  P = d$P
  PET = d$E
  Robs = d$Robs
  create_GR4J(date, P, PET, Robs, inds_calib, inds_valid, ...)
}

#' @export
create_GR4J.default <- function(
    date, P, PET, Robs,
    inds_calib = NULL, inds_valid = NULL, ...) {

  InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J,
    DatesR = as.POSIXct(date), Precip = P, PotEvap = PET)

  RunOptions <- CreateRunOptions(
    FUN_MOD = RunModel_GR4J,
    InputsModel = InputsModel, IndPeriod_Run = inds_calib,
    IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = NULL
  ) # 前一年作为预热期

  RunOptions_valid = RunOptions
  RunOptions_valid$IndPeriod_Run = inds_valid

  InputsCrit <- CreateInputsCrit(
    FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
    RunOptions = RunOptions, VarObs = "Q", Obs = Robs[inds_calib]
  )

  CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)
  OutputsCalib <- Calibration_Michel(
    InputsModel = InputsModel, RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions,
    FUN_MOD = RunModel_GR4J
  )
  Param <- OutputsCalib$ParamFinalR

  # 这里需要划分训练期和验证期
  out_calib <- RunModel_GR4J(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)

  out_valid <- RunModel_GR4J(InputsModel = InputsModel, RunOptions = RunOptions_valid, Param = Param)

  data = rbind(
    data.table(type = "calib", date = date[inds_calib], 
      Rsim = out_calib$Qsim, Robs = Robs[inds_calib]),
    data.table(type = "valid", date = date[inds_valid], 
      Rsim = out_valid$Qsim, Robs = Robs[inds_valid])
  )
  gof = rbind(
    cbind(type = "calib", GOF(Robs[inds_calib], out_calib$Qsim)),
    cbind(type = "valid", GOF(Robs[inds_valid], out_valid$Qsim))
  )
  
  listk(data = data, gof = gof, 
    model = listk(par = Param, RunOptions, InputsModel, name = "GR4J")) |> 
      set_class("OUTPUT_HydroALL")
}

