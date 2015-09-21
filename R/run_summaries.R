#' get summary of key information from dmp.txt file for a given directory
#' @param dir directory of dmp.txt file
#' @export
get_dmp_summary <- function(dir = getwd()) {
    runsum <- source(normalize_file(dir, "dmp.txt"))$value
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
    file_path <- normalize_file(dir,mdl)
    file_docs <- parse_multidoc_yaml(file_path)
    output_list <- lapply(file_docs, function(x) {
        yaml::yaml.load(paste(x, collapse = "\n"))
    })
    output <- purrr::flatten(output_list)
    names(output)[names(output)==""] <- "source"
    return(output)
}