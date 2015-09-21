#' parse the progress.txt file to a dataframe with values by iteration
#' @param dir directory of progress file
#' @param file file name of progress.txt
#' @return data.frame with column names param, value, iteration
#' @export
parse_progress <- function(dir, file = "progress.txt") {

   file_lines <- readr::read_lines(normalize_file(dir, file))
   iteration_indices <- c(1:length(file_lines))[stringr::str_detect(file_lines,
                                                                    "Iteration")]
   iteration_indices <- c(iteration_indices, length(file_lines))

   iteration_list <- list()
   for (i in seq_along(iteration_indices)) {
       if (i == length(iteration_indices)) {
           break ## because always using i+1 so want to kill it 1 step early
       }
       start_index <- iteration_indices[i]
       end_index <- iteration_indices[i +1]
       iteration_list[[i]] <- file_lines[(start_index+1):(end_index-1)]
   }

    iteration_parsed_list <- lapply(seq_along(iteration_list), function(x, iteration_list) {
        parsed_iterations <- stringr::str_replace_all(iteration_list[[x]], "\\(|\\)|\\t", "")
        iteration_df <- data.frame(output = parsed_iterations[-length(parsed_iterations)])
        output <- tidyr::separate(iteration_df, col = output, sep = "=", into = c("param", "value"))
        output$iteration <- x
        return(output)
    }, iteration_list)

    return(dplyr::bind_rows(iteration_parsed_list))
}
