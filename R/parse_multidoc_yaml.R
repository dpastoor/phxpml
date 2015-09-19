extract_dashes <- function(x) {
    return(x[-grep("---", x)])
}

#' parse multi-document yaml to single document
#' @param filename
#' @export
#' @details
#' yaml_load is unable to parse multiline markdown documents, this
#' extracts each individual document to a list, so the list can be
#' passed to the yaml parser.
#'
parse_multidoc_yaml <- function(file, ...) {
    lines <- readr::read_lines(file, ...)
    breaks <- grep("---", lines)
    splits <- unique(c(1, breaks, length(lines)))
    docs <- list()
    for (i in seq_along(splits)) {
        if (i == length(splits)) {
            break
        }
    docs[[i]] <- extract_dashes(lines[(splits[i]):(splits[i+1])])
    }
    return(docs)
}