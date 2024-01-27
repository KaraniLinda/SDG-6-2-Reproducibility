#' JMP raw sanitation data
#'
#' This dataset contains raw primary and secondary sanitation estimates.
#'
#' @format A data frame with 16576 rows and 9 variables:
#' \describe{
#'   \item{source}{Represents the code associated with the source of the data i.e census, report or survey}
#'   \item{type}{Type of the data i.e census, survey or report}
#'   \item{year}{Represents the year when the estimate was recorded}
#'   \item{var_short}{Short variable sanitation description }
#'   \item{value}{Represents the percentage pf the population using a particular sanitation service}
#'   \item{iso3}{ISO3 country code}
#'   \item{var_long}{Represents the sanitation service}
#'   \item{residence}{Rural or urban residence}
#'   \item{san_service_chain}{Represents the service ladder associated with the sanitation indicator}
#' }
#'
#' @name jmpraw
#' @rdname jmpraw
#' @export
jmpraw <- readr::read_rds("/Users/lindakarani/Documents/gitrepo/SDG-6-2-Reproducibility/data/2020-09-30_jmp_sanitation_raw_data.rds")
