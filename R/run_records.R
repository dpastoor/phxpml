get_last_model_dir <- function(dir_list, models) {
    results <- vapply(models, function(x, dir_list) {
        model_runs <- dir_list[grepl(paste0(x, "_"), dir_list)]
        est_nums <- as.numeric(gsub(paste0(x, "_", "est_"), "", model_runs))
        return(model_runs[est_nums == max(est_nums)])
    }, character(1), dir_list)
    return(results)
}

# dir_list <- c("theo002_est_3", "theo001_est_1","theo001_est_10", "theo001_est_2")
# models <- c("theo001", "theo002")
# result <- c(theo001 = "theo001_est_10", theo002 = "theo002_est_3")
# all.equal(get_last_model_dir(dir_list, models),result)

#' @param dir directory to search
#' @param recursive boolean
#' @param filter vector of run names to filter
#' @return list of dir, yml_models, model_dirs, and time of scan
#' @export
get_run_records <- function(dir = getwd(),
                            recursive = F,
                            filter= NULL) {
    dir <- normalizePath(dir)
    yml_list <- list_files_with_exts(dir,
                                     exts = c("yaml", "yml"),
                                     full.names = F)
    dir_list <- list.dirs(dir, full.names=F)
    models <- gsub("\\.yml|\\.yaml", "", yml_list)
    last_model_dirs <- get_last_model_dir(dir_list, models)

    result_list <- list(
        dir = dir,
        yml_models = yml_list,
        model_dirs = last_model_dirs,
        time = Sys.time()
        )
    return(result_list)
}



