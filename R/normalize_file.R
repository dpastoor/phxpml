normalize_file <- function(dir, file) {
    return(normalizePath(file.path(dir, file)))
}