normalize_file <- function(dir, file) {
    return(file.path(normalizePath(dir), file))
}