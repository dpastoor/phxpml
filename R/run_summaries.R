#' get summary of key information from dmp.txt file for a given directory
#' @param dir directory of dmp.txt file
#' @export
get_dmp_summary <- function(dir = getwd()) {
    runsum <- source(file.path(normalizePath(dir), "dmp.txt"))$value
    attributes(runsum)$class <- NULL
    key_runsum <- list(fixed = runsum$coefficients$fixed,
                       logLik = runsum$logLik,
                       returnCode = runsum$returnCode,
                       omega = runsum$omega,
                       eta_shrinkage = runsum$eta[,"shrinkage"],
                       sigma = runsum$sigma)
    return(key_runsum)
}

#' get the parsed yml from the model file
#' @param mdl model file
#' @param dir directory of model file
#' @export
get_model_summary <- function(mdl, dir = getwd()) {
    return(yaml::yaml.load_file(file.path(normalizePath(dir),mdl)))
}