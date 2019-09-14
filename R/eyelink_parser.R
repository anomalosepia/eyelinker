#' Read EyeLink ASC Files
#'
#' Imports data from EyeLink .ASC files into (relatively) tidy data frames for analysis and
#' visualization. Event data and/or raw sample data from the files can be imported, along with
#' information about the tracker hardware and configuration. All data is divided into numbered
#' blocks using the "START" and "END" messages in the ASC file.
#'
#' ASC files can contain anywhere between 125 to 2000 rows of samples for every second of recording,
#' meaning that the resulting files can be very large (1.2 million rows of samples for 20 minutes at
#' 1000Hz). As a result, importing some ASC files can be slow, and the resulting data frames can
#' take up 100's of MB of memory. To speed up import and greatly reduce memory load, you can choose
#' to ignore raw samples and only import events by setting the samples parameter to \code{FALSE}.
#'
#' This function returns a list containing the following possible data frames: \describe{
#'   \item{raw}{The raw sample data from the tracker, containing gaze and pupil data}
#'   \item{sacc}{Contains data from saccade end events}
#'   \item{fix}{Contains data from fixation end events}
#'   \item{blinks}{Contains data from blink end events}
#'   \item{msg}{Contains messages sent or received by the tracker during recording}
#'   \item{input}{Contains data from input events}
#'   \item{button}{Contains data from button press events}
#'   \item{info}{Contains information about the tracker settings and configuration}
#' }
#' The names of the columns in these data frames correspond to column names given in the ASC
#' section of the EyeLink 1000 User's Guide.
#'
#' Note that this function cannot import EDFs directly, they must be converted to plain-text ASC
#' using the edf2asc utilty before importing.
#'
#' @usage
#' read.asc(fname, samples = TRUE, events = TRUE)
#'
#' @param fname \code{character} vector indicating the name of the .asc file to import.
#' @param samples \code{logical} indicating whether raw sample data should be imported. Defaults
#'   to \code{TRUE}.
#' @param events \code{logical} indicating whether event data (e.g. saccades, blinks, messages,
#'    etc.) should be imported. Defaults to \code{TRUE}.
#' @return A \code{list} of \code{\link[tibble]{tibble}}s containing data from the .asc file.
#'
#' @author Simon Barthelme & Austin Hurst
#' @examples
#' # Example file from SR research that ships with the package
#' fpath <- system.file("extdata/mono500.asc.gz", package = "eyelinker")
#' dat <- read.asc(fpath)
#' plot(dat$raw$time, dat$raw$xp, xlab = "Time (ms)", ylab = "Eye position along x-axis (pix)")
#'
#' @export read.asc

# TODO:
#  - Check for multiple unique RECCFG lines, throw an error if so (can this even happen?)
#  - Add dummy data frames when data missing, so things like map_df can work
#  - Add parsing of calibration & drift correct info
#  - Freshen up documentation & vignettes
#  - Add function for reading multiple ASCs at once?
#  - Add function for parsing button samples? They're stored in a weird format
#  - Improve testthat usage w/ more comprehensive testing



read.asc <- function(fname, samples = TRUE, events = TRUE) {

    inp <- read_lines(fname)

    # Convert to ASCII
    inp <- stri_enc_toascii(inp)

    # Get strings prior to first tab for each line for faster string matching
    inp_first <- stri_split_regex(inp, "\\s", 2, simplify = TRUE)[, 1]

    # Check if any actual data recorded in file
    starts <- inp_first == "START"
    if (!any(starts)) {
        stop("No samples or events found in .asc file.")
    }

    # Read metadata from file before processing
    is_raw <- str_detect(inp_first, "^[0-9]")
    info <- get_info(inp[!is_raw])

    # Do some extra processing/sanitizing if there's HTARG info in the file
    if (info$htarg) {
        ret <- handle_htarg(inp, info, any(is_raw))
        inp <- ret[[1]]
        info <- ret[[2]]
    }

    # Find blocks and discard lines between block ENDs and next block STARTs
    dividers <- c(which(starts), length(inp))
    block <- cumsum(starts)
    for (i in 2:length(dividers)) {
        start <- dividers[i - 1]
        end <- dividers[i] - 1
        endline <- which(inp_first[start:end] == "END") + start
        if (length(endline) > 0) {
            block[endline[1]:end] <- NA
        }
    }
    in_block <- block != 0 & !is.na(block)
    inp <- inp[in_block]
    inp_first <- inp_first[in_block]
    is_raw <- is_raw[in_block]
    block <- block[in_block]

    # Initialize list of data output and process different data types
    out <- list()
    if (samples) {
        if (any(is_raw)) out$raw <- process_raw(inp[is_raw], block[is_raw], info)
    }
    if (events) {
        is_sacc <- inp_first == "ESACC"
        if (any(is_sacc)) out$sacc <- process_saccades(inp[is_sacc], block[is_sacc], info)

        is_fix <- inp_first == "EFIX"
        if (any(is_fix)) out$fix <- process_fixations(inp[is_fix], block[is_fix], info)

        is_blink <- inp_first == "EBLINK"
        if (any(is_blink)) out$blinks <- process_blinks(inp[is_blink], block[is_blink])

        is_msg <- inp_first == "MSG"
        if (any(is_msg)) out$msg <- process_messages(inp[is_msg], block[is_msg])

        is_input <- inp_first == "INPUT"
        if (any(is_input)) out$input <- process_input(inp[is_input], block[is_input])

        is_button <- inp_first == "BUTTON"
        if (any(is_button)) out$button <- process_buttons(inp[is_button], block[is_button])
    }
    #if (saccades) {
    #    is_sacc <- inp_first == "ESACC"
    #    if (any(is_sacc)) out$sacc <- process_saccades(inp[is_sacc], block[is_sacc], info)
    #}
    #if (fixations) {
    #    is_fix <- inp_first == "EFIX"
    #    if (any(is_fix)) out$fix <- process_fixations(inp[is_fix], block[is_fix], info)
    #}
    #if (blinks) {
    #    is_blink <- inp_first == "EBLINK"
    #    if (any(is_blink)) out$blinks <- process_blinks(inp[is_blink], block[is_blink])
    #}
    #if (msgs) {
    #    is_msg <- inp_first == "MSG"
    #    if (any(is_msg)) out$msg <- process_messages(inp[is_msg], block[is_msg])
    #}
    #if (input) {
    #    is_input <- inp_first == "INPUT"
    #    if (any(is_input)) out$input <- process_input(inp[is_input], block[is_input])
    #}
    #if (buttons) {
    #    is_button <- inp_first == "BUTTON"
    #    if (any(is_button)) out$button <- process_buttons(inp[is_button], block[is_button])
    #}
    out$info <- info

    out
}


process_raw <- function(raw, blocks, info) {

    # Determine if timestamps stored as floats (edf2asc option -ftime, useful for 2000 Hz)
    first_time <- strsplit(raw[1], "\\s+")[[1]][1]
    float_time <- str_detect(first_time, "\\.")

    # Generate column names and types based in info in header
    colinfo <- get_raw_header(info, float_time)
    raw.colnames <- colinfo$names
    raw.coltypes <- colinfo$types

    # Discard any rows with missing columns (usually rows where eye is missing)
    row_length <- stri_count_fixed(raw, "\t") + 1
    max_length <- max(row_length)
    raw <- raw[row_length == max_length]
    blocks <- blocks[row_length == max_length]

    # Verify that generated columns match up with actual maximum row length
    length_diff <- max_length - length(raw.coltypes)
    if (length_diff > 0) {
        warning(paste(
            "Unknown columns in raw data.",
            "Assuming first one is time, please check the others"
        ))
        raw.colnames <- c(raw.colnames, paste0("X", 1:length_diff))
        raw.coltypes <- c(raw.coltypes, rep("c", length_diff))
    }

    # Process raw sample data using readr
    coltypes <- paste0(raw.coltypes, collapse = "")
    raw_df <- read_tsv(raw, col_names = raw.colnames, col_types = coltypes, na = ".", progress = F)
    
    # Append block numbers to beginning of data frame
    raw_df <- add_column(raw_df, block = blocks, .before = 1)

    # Replace missing pupil data (zeros) with NAs
    if (info$mono) {
        raw_df$ps[raw_df$ps == 0] <- NA
    } else {
        raw_df$psl[raw_df$psl == 0] <- NA
        raw_df$psr[raw_df$psr == 0] <- NA
    }

    raw_df
}


process_saccades <- function(saccades, blocks, info) {

    # Parse saccade data, dropping useless "ESACC" first column
    if (length(saccades) == 1) saccades <- c(saccades, "")
    sacc_df <- read_table2(saccades, col_names = FALSE, na = ".")[, -1]
    names(sacc_df) <- get_sacc_header(info)

    # Move eye col to end & make factor, append block numbers to beginning of data frame
    sacc_df <- sacc_df[, c(2:ncol(sacc_df), 1)]
    sacc_df$eye <- factor(sacc_df$eye, levels = c("L", "R"))
    sacc_df <- add_column(sacc_df, block = blocks, .before = 1)

    # Set amplitudes for any saccades missing start/end coords to NAs because they're wonky
    ampl_cols <- which(str_detect(names(sacc_df), "ampl"))
    sacc_df[is.na(sacc_df$sxp) | is.na(sacc_df$exp), ampl_cols] <- NA

    sacc_df
}


process_fixations <- function(fixations, blocks, info) {

    # Parse fixation data, dropping useless "EFIX" first column
    if (length(fixations) == 1) fixations <- c(fixations, "")
    fix_df <- read_table2(fixations, col_names = FALSE, na = ".")[, -1]
    names(fix_df) <- get_fix_header(info)

    # Move eye col to end & make factor, append block numbers to beginning of data frame
    fix_df <- fix_df[, c(2:ncol(fix_df), 1)]
    fix_df$eye <- factor(fix_df$eye, levels = c("L", "R"))
    fix_df <- add_column(fix_df, block = blocks, .before = 1)

    fix_df
}


process_blinks <- function(blinks, blocks) {

    # Parse and name blink data, dropping useless "EBLINK" first column
    if (length(blinks) == 1) blinks <- c(blinks, "")
    blink_df <- read_table2(blinks, col_names = FALSE)[, -1]
    names(blink_df) <- c("eye", "stime", "etime", "dur")

    # Move eye col to end & make factor, append block numbers to beginning of data frame
    blink_df <- blink_df[, c(2:ncol(blink_df), 1)]
    blink_df$eye <- factor(blink_df$eye, levels = c("L", "R"))
    blink_df <- add_column(blink_df, block = blocks, .before = 1)

    blink_df
}


process_messages <- function(msgs, blocks) {

    # Process messages from tracker (needs stringi import)
    msg_mat <- stri_split_fixed(msgs, " ", 2, simplify = TRUE)
    msg_mat[, 1] <- substring(msg_mat[, 1], first = 5)
    msg_df <- as_tibble(msg_mat, .name_repair = ~ c("time", "text"))
    msg_df$time <- as.numeric(msg_df$time)

    # Append block numbers to beginning of data frame
    msg_df <- add_column(msg_df, block = blocks, .before = 1)

    msg_df
}


process_input <- function(input, blocks) {

    # Parse and name input data, dropping useless "INPUT" first column
    if (length(input) == 1) input <- c(input, "")
    input_df <- read_table2(input, col_names = FALSE)[, -1]
    names(input_df) <- c("time", "value")

    # Append block numbers to beginning of data frame
    input_df <- add_column(input_df, block = blocks, .before = 1)

    input_df
}


process_buttons <- function(button, blocks) {

    # Parse and name button data, dropping useless "BUTTON" first column
    if (length(button) == 1) button <- c(button, "")
    button_df <- read_table2(button, col_names = FALSE)[, -1]
    names(button_df) <- c("time", "button", "state")

    # Append block numbers to beginning of data frame
    button_df <- add_column(button_df, block = blocks, .before = 1)

    button_df
}


handle_htarg <- function(inp, info, any_raw) {

    # First, check if any normal htarg in input because that's much faster
    info$htarg <- any(str_detect(inp, fixed(".............")))

    if (!info$htarg) {
        # Normally the htarg stuff is just twelve dots in a row, but in case
        # there are errors we need the following regexp.
        pat <- paste0(
            "(M|\\.)(A|\\.)(N|\\.)(C|\\.)(F|\\.)(T|\\.)(B|\\.)",
            "(L|\\.)(R|\\.)(T|\\.)(B|\\.)(L|\\.)(R|\\.)"
        )
        info$htarg <- any(str_detect(inp, pat))
    }

    # Just to spite us, there's an inconsistency in how HTARG info is encoded (missing tab).
    # We fix it if necessary.
    if (info$htarg && any_raw) {
        inp <- str_replace_all(inp, fixed("............."), fixed("\t............."))
    }

    list(inp, info)
}


# Gets useful metadata about the tracker setup & settings from the file
get_info <- function(nonsample) {

    header <- nonsample[str_detect(nonsample, "^\\*\\*")]
    info <- data.frame(
        date = NA, model = NA, version = NA, sample.rate = NA, cr = NA,
        left = NA, right = NA, mono = NA, screen.x = NA, screen.y = NA,
        mount = NA, filter.level = NA, sample.dtype = NA, event.dtype = NA,
        velocity = NA, resolution = NA, htarg = NA, input = NA, buttons = NA
    )

    # Get date/time of recording from file
    info$date <- as.POSIXct(from_header(header, "DATE"), format = "%a %b %d %H:%M:%S %Y")

    # Get tracker model/version info
    version_info <- get_model(header)
    info$model <- version_info[1]
    info$version <- version_info[2]

    # Get tracker mount info
    elclcfg <- nonsample[str_detect(nonsample, fixed("ELCLCFG"))]
    if (length(elclcfg) > 0) {
        info$mount <- get_mount(gsub('.* ELCLCFG\\s+(.*)', '\\1', elclcfg[1]))
    }

    # Get display size from file
    screen_res <- get_resolution(nonsample)
    info$screen.x <- screen_res[1]
    info$screen.y <- screen_res[2]

    # Find the samples and events config lines in the non-sample input
    config_all <- nonsample[str_detect(nonsample, "^EVENTS|^SAMPLES")]
    if (any(str_detect(config_all, "^EVENTS"))) {
        config <- rev(config_all[str_detect(config_all, "^EVENTS")])[1]
        info$event.dtype <- strsplit(config, "\\s+")[[1]][2]
    }
    if (any(str_detect(config_all, "^SAMPLES"))) {
        config <- rev(config_all[str_detect(config_all, "^SAMPLES")])[1]
        info$sample.dtype <- strsplit(config, "\\s+")[[1]][2]
    }
    if (length(config_all) > 0) {
        info$sample.rate <- ifelse(
            grepl('RATE', config),
            as.numeric(gsub('.*RATE\\s+([0-9]+\\.[0-9]+).*', '\\1', config)),
            NA
        )
        info$cr <- grepl("\tCR", config)
        info$filter.level <- ifelse(
            grepl('FILTER', config),
            as.numeric(gsub('.*FILTER\\s+([0-9]).*', '\\1', config)),
            NA
        )
        info$velocity <- grepl("\tVEL", config)
        info$resolution <- grepl("\tRES", config)
        info$htarg <- grepl("\tHTARG", config)
        info$input <- grepl("\tINPUT", config)
        info$buttons <- grepl("\tBUTTONS", config)
        info$left <- grepl("\tLEFT", config)
        info$right <- grepl("\tRIGHT", config)
        info$mono <- !(info$right & info$left)
    }

    info
}


# Extracts the value of a given field from the ASC header
from_header <- function(header, field) {
    s <- header[grepl(paste0("\\*\\* ", field), header)]
    ifelse(length(s) > 0, gsub(paste0("\\*\\* ", field, ": (.*)"), "\\1", s), NA)
}


# Get the display resolution of the stimulus computer from the file
get_resolution <- function(nonsample) {

    res <- c(NA, NA)
    for (pattern in c("DISPLAY_COORDS", "GAZE_COORDS", "RESOLUTION")) {
        display_xy <- nonsample[str_detect(nonsample, fixed(pattern))]
        if (length(display_xy) == 0) next
        display_xy <- gsub(paste0(".* ", pattern, "\\s+(.*)"), "\\1", display_xy[1])
        display_xy <- as.numeric(unlist(strsplit(display_xy, split = "\\s+")))
        res <- c(display_xy[3] - display_xy[1] + 1, display_xy[4] - display_xy[2] + 1)
    }

    res
}


# Gets the model and software version of the tracker based on the header contents
get_model <- function(header) {

    version_str <- from_header(header, "VERSION")
    version_str2 <- header[grepl("\\*\\* EYELINK II", header)]

    if (is.na(version_str)) {
        model <- "Unknown"
        ver_num <- "Unknown"
    } else if (version_str != 'EYELINK II 1') {
        model <- "EyeLink I"
        ver_num <- gsub('.* ([0-9]\\.[0-9]+) \\(.*', '\\1', version_str)
    } else {
        ver_num <- gsub('.* v(.*) [[:alpha:]].*', '\\1', version_str2)
        model <- ifelse(as.numeric(ver_num) < 2.4,
            'EyeLink II',
            ifelse(as.numeric(ver_num) < 5,
                'EyeLink 1000',
                ifelse(as.numeric(ver_num) < 6,
                    'EyeLink 1000 Plus',
                    'EyeLink Portable Duo'
                )
            )
        )
    }

    return(c(model, ver_num))
}


# Get info about the camera mount type for desk-mounted EyeLinks
get_mount <- function(mount_str) {

    mounts <- list(
        "MTABLER" = "Desktop / Monocular / Head Stabilized",
        "BTABLER" = "Desktop / Binocular / Head Stabilized",
        "RTABLER" = "Desktop / Monocular / Remote",
        "RBTABLER" = "Desktop / Binocular / Remote",
        "AMTABLER" = "Arm Mount / Monocular / Head Stabilized",
        "ARTABLER" = "Arm Mount / Monocular / Remote",
        "TOWER" = "Tower Mount / Monocular / Head Stabilized",
        "BTOWER" = "Tower Mount / Binocular / Head Stabilized",
        "MPRIM" = "Primate Mount / Monocular / Head Stabilized",
        "BPRIM" = "Primate Mount / Binocular / Head Stabilized",
        "MLRR" = "Long-Range Mount / Monocular / Head Stabilized",
        "BLRR" = "Long-Range Mount / Binocular / Head Stabilized"
    )
    mount <- ifelse(mount_str %in% names(mounts), mounts[[mount_str]], NA)

    mount
}


# Generate column names/types for raw sample data based on gathered info
get_raw_header <- function(info, float_time) {

    eyev <- c("xp", "yp", "ps")
    ctype <- rep("d", 3)

    if (!info$mono) {
        eyev <- c(paste0(eyev, "l"), paste0(eyev, "r"))
        ctype <- rep(ctype, 2)
    }
    if (info$velocity) {
        vel <- ifelse(info$mono, c("xv", "yv"), c("xvl", "yvl", "xvr", "yvr"))
        eyev <- c(eyev, vel)
        ctype <- c(ctype, rep("d", length(vel)))
    }
    if (info$resolution) {
        eyev <- c(eyev, "xr", "yr")
        ctype <- c(ctype, rep("d", 2))
    }
    if (info$input) {
        eyev <- c(eyev, "input")
        ctype <- c(ctype, "d")
    }
    if (info$buttons) {
        eyev <- c(eyev, "buttons")
        ctype <- c(ctype, "d")
    }
    if (info$cr) {
        # With corneal reflections we need an extra column
        eyev <- c(eyev, "cr.info")
        ctype <- c(ctype, "c")
    }
    if (info$htarg) {
        # Three extra columns for remote set-up
        eyev <- c(eyev, "tx", "ty", "td", "remote.info")
        ctype <- c(ctype, c("d", "d", "d", "c"))
    }

    list(names = c("time", eyev), types = c(ifelse(float_time, "d", "i"), ctype))
}


get_event_header <- function(info, xy_cols) {

    base <- c("eye", "stime", "etime", "dur")
    # If event data type is HREF, events contain both HREF and GAZE data, so there are extra columns
    if (info$event.dtype == "HREF") {
        xy_cols <- c(paste0("href.", xy_cols), xy_cols)
    }
    if (info$res) {
        xy_cols <- c(xy_cols, "xr", "yr")
    }

    c(base, xy_cols)
}


get_sacc_header <- function(info) {
    get_event_header(info, c("sxp", "syp", "exp", "eyp", "ampl", "pv"))
}


get_fix_header <- function(info) {
    get_event_header(info, c("axp", "ayp", "aps"))
}
