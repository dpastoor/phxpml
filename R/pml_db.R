read_csv_dir <- function(dir, file, ...) {
    readr::read_csv(normalize_file(dir, file), ...)
    }

#' factory method for processing files in a directory
#' @examples \dontrun{
#' file_processor <- process_files() # initialize
#' # can then access different processing functions based on file of interest
#' file_processor$dmp$processing_fun(dir)
#' file_processor$IdEta$processing_fun(dir)
#' }
#' @export
process_files <- function() {
    all_files <- list(
    dmp      = list(filename = "dmp.txt",
                    processing_fun = function(dir) {
                        return(get_dmp_summary(dir))
                        }),
    doses    = list(filename = "doses.csv",
                    processing_fun = function(dir) {
                        return(read_csv_dir(dir, "doses.csv"))
                    }),
    EtaCov   = list(filename = "EtaCov.txt",
                    processing_fun = function(dir) {
                        output <- read_csv_dir(dir, "EtaCov.txt", col_names=FALSE)
                        names(output) <- c("ID1", "ID2", "ID3", "ID4", "ID5",
                                           "EtaName", "CovrName", "Eta", "Covr")
                        # has not been checked if has NPEta like in GUI
                        return(output)
                    }),
    EtaEta   = list(filename = "EtaEta.txt",
                    processing_fun = function(dir) {
                        output <- read_csv_dir(dir, "EtaEta.txt", col_names=FALSE)
                        names(output) <- c("ID1", "ID2", "ID3", "ID4", "ID5",
                                           "Eta1Name", "Eta2Name", "Eta1", "Eta2")
                        return(output)
                    }),
    IdEta    = list(filename = "IdEta.txt",
                    processing_fun = function(dir) {
                        output <- read_csv_dir(dir, "IdEta.txt", col_names=FALSE)
                        names(output) <- c("ID1", "ID2", "ID3", "ID4", "ID5",
                                           "Eta", "Value")
                        return(output)
                    }),
    iniest   = list(filename = "iniest.csv",
                    processing_fun = function(dir) {
                        return(read_csv_dir(dir, "iniest.csv"))
                    }),
    out      = list(filename = "out.txt",
                    processing_fun = parse_outtxt),
    stderr   = list(filename = "stderr_smat.dat",
                    processing_fun = function(dir) {
                        output <- readr::read_lines(normalize_file(dir, "stderr_smat.dat"), skip = 1)
                        # weirdly shaped file, so trim whitespace, replace all whitespace with single comma delim,
                        # collapse to single string with line breaks then parse as csv
                        output <- stringi::stri_trim_both(output)
                        parsed_output <- readr::read_csv(
                            paste0(
                                stringi::stri_replace_all(output,
                                                          replacement = ",",
                                                          regex = "\\s+"),
                                collapse = "\n"),
                            col_names=FALSE
                            )
                        names(parsed_output) <- c("param_num", "value", "std_err", "rel_std_err")
                        return(parsed_output)
                    }),
    StrCov   = list(filename = "StrCov.txt",
                    processing_fun = function(dir) {
                        output <- read_csv_dir(dir, "StrCov.txt", col_names=FALSE)
                        names(output) <- c("ID1", "ID2", "ID3", "ID4", "ID5",
                                           "CovrName", "StrName", "Str", "Covr")
                        return(output)
                    }),
    VarCoVar = list(filename = "VarCoVar.csv",
                    processing_fun = function(dir) {
                        output <- read_csv_dir(dir, "VarcoVar.csv")
                        output_matrix <- as.matrix(output[2:length(output)], ncol = length(output)-1)
                        dimnames(output_matrix) <- list(names(output)[-1], names(output)[-1])
                        return(output_matrix)
                    })
    )

    return(all_files)
}


#' create a pmldb instance
#' @param dir directory containing files of interest
#' @param savedb save created list as a .rds file in the output dir
#' @param invalidate overwrite existing .rds db file if it exists
#' @export
create_pmldb <- function(dir,
                         savedb = TRUE,
                         invalidate = FALSE
                         ) {

    dir <- normalizePath(dir)
    db_name <- paste0(basename(dir), ".rds")

    if (file.exists(normalize_file(dir, db_name)) && !invalidate) {
        message(paste0("found cached database file: ", db_name))
        return(readRDS(normalize_file(dir, db_name)))
    }

    file_processing <- process_files()

    init_db <- list(
        dmp = NULL,
        doses = NULL,
        EtaCov = NULL,
        EtaEta = NULL,
        IdEta = NULL,
        IniCovr = NULL,
        iniest = NULL,
        LLbysub = NULL,
        out = NULL,
        stderr = NULL,
        StrCov = NULL,
        VarCoVar = NULL
    )


    for (i in seq_along(file_processing)) {
        file_name <- file_processing[[i]]$filename

        if(file.exists(normalize_file(dir, file_name))) {
            message(paste("found and processing", file_name, "..."))
            init_db[[names(file_processing)[i]]] <- file_processing[[i]]$processing_fun(dir)
        }
    }

    attr(init_db, "class") <- "pmldb"

    if(savedb) {
        message(paste0("saving rds db as: ", db_name))
        saveRDS(init_db, normalize_file(dir, db_name))
    }
    return(init_db)
}

pmldb <- create_pmldb(dir)

