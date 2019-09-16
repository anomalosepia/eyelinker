#' eyelinker: read raw data from EyeLink eye trackers into tidy(ish) data
#'
#' Eyelink eye trackers output a horrible mess, typically under
#'  the form of an .asc file. The file in question is an assorted collection of
#'  messages, events and raw data. This R package will attempt to make sense of it.
#'
#' The main function in the package is read.asc.
#' @docType package
#' @name eyelinker
NULL

#' @importFrom stringr str_sub<- str_detect fixed
#' @importFrom tibble as_tibble add_column
#' @importFrom readr read_lines read_tsv read_table2
#' @importFrom stringi stri_enc_toascii stri_split_regex stri_split_fixed stri_count_fixed
#' @importFrom intervals which_nearest distance_to_nearest Intervals
NULL
