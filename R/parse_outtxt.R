#' extract key output the out.txt file
#' @param dir directory of out.txt file
#' @param file filename defaults to out.txt
#' @export
#' @return list of residuals and key outputs
parse_outtxt <- function(dir, file = "out.txt") {
   file_lines <- readr::read_lines(normalize_file(dir, file))


   theta_indices <- stringr::str_detect(file_lines, "theta")
   theta_lines <- file_lines[theta_indices]
   theta_list <- stringr::str_split(stringr::str_trim(theta_lines), "=|#")
   theta_vec <- sapply(theta_list, function(x) {
     value <- PKPDmisc::as_numeric(stringr::str_trim(gsub("\t", "", x[2])))
     names(value) <- x[3]
     return(value)
   })
   residuals <- readr::read_tsv(
                   paste0(
                       file_lines[(grep("residuals", file_lines)+1):length(file_lines)],
                       collapse = "\n")
               )
   residuals <- tidyr::separate(residuals, `ID1  ID2  ID3  ID4  ID5`, sep = "\\s+",
                                into = c("ID1",
                                       "ID2",
                                       "ID3",
                                       "ID4",
                                       "ID5"
                                        )
                                )
   # turn character strings into factor and keep original order
   # if just do factor, "10" will come before "2"
   residuals$ID5 <- factor(residuals$ID5, levels = unique(residuals$ID5))
   return(list(residuals = residuals,
               theta = theta_vec))

}
