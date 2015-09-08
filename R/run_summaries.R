#' @param dir directory of dmp.txt file
#' @export
get_dmp_summary <- function(dir) {
    runsum <- source(paste0(normalizePath(dir), "dmp.txt"))$value
    attributes(runsum)$class <- NULL
    key_runsum <- list(fixed = runsum$coefficients$fixed,
                       logLik = runsum$logLik,
                       returnCode = runsum$returnCode,
                       omega = runsum$omega,
                       eta_shrinkage = runsum$eta[,"shrinkage"],
                       sigma = runsum$sigma)
    return(key_runsum)
}

#' @param mdl model file
#' @param dir directory of model file
#' @export
get_model_summary <- function(mdl, dir) {
    return(yaml::yaml.load_file(paste0(normalizePath(dir),mdl)))
}