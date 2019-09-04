
##' Read EyeLink ASC file
##'
##' ASC files contain raw data from EyeLink eyetrackers (they're ASCII versions of the raw binaries
##' which are themselves in EDF format). This utility tries to parse the data into something that's
##' usable in R. If all you have is an EDF file, you need to convert it first using the edf2asc
##' utility included in the EyeLink API kit.
##'
##' Please read the EyeLink manual before using it for any serious work, very few checks are done to
##' see if the output makes sense. read.asc will return data frames containing a "raw" signal as
##' well as event series. Events are either system signals (triggers etc.), which are stored in the
##' "msg" field, or correspond to the EyeLink's interpretation of the eye movement traces
##' (fixations, saccades, blinks). ASC files are divided into blocks signaled by START and END
##' signals. The block structure is reflected in the "block" field of the dataframes. The names of
##' the various columns are the same as the ones used in the EyeLink manual, with two exceptions:
##' "cr.info", which gives you information about corneal reflection tracking, and "remote.info",
##' which gives you information about the state of the remote setup (if applicable). A series of
##' periods (e.g. ".....") for either indicates that tracking is functioning properly, other values
##' indicate problems with CR or remote tracking for that sample. Refer to the manual for details.
##'
##' @title Read EyeLink ASC file
##' @param fname file name
##' @return a list with components
##'   raw: raw eye positions, velocities, resolution, etc.
##'   msg: messages (no attempt is made to parse them)
##'   fix: fixations
##'   blinks: blinks
##'   sacc: saccades
##'   info: metadata
##'
##' @author Simon Barthelme
##' @examples
##' # Example file from SR research that ships with the package
##' fpath <- system.file("extdata/mono500.asc.gz", package = "eyelinker")
##' dat <- read.asc(fpath)
##' plot(dat$raw$time, dat$raw$xp, xlab = "Time (ms)", ylab = "Eye position along x-axis (pix)")
##' @export

read.asc <- function(fname) {

    inp <- readLines(fname)

    # Convert to ASCII
    inp <- stri_enc_toascii(inp)

    # Check if any actual data recorded in file
    if (!any(str_detect(inp, "^START"))) {
        stop("No samples or events found in .asc file.")
    }

    # Read metadata from file before processing
    info <- getInfo(inp)
    has.raw <- length(str_select(inp, "^SAMPLES") > 0)

    # Filter out empty lines, comments, trailing whitespace
    inp <- str_select(inp, "^\\w*$", reverse = TRUE) %>%
        str_select("^#", reverse = TRUE) %>%
        str_select("^/", reverse = TRUE) %>%
        str_trim(side = "right")

    # Just to spite us, there's an inconsistency in how HTARG info is encoded (missing tab).
    # We fix it if necessary.
    if (has.raw && info$htarg) {
        inp <- str_replace_all(inp, fixed("............."), fixed("\t............."))
    }

    # "Header" isn't strict, it's whatever comes before the first "START" line
    init <- str_detect(inp, "^START") %>% which %>% min
    header <- inp[1:(init - 1)]
    inp <- inp[init:length(inp)]

    # Find blocks
    bl.start <- str_detect(inp, "^START") %>% which
    bl.end <- str_detect(inp, "^END") %>% which
    nBlocks <- length(bl.start)
    blocks <- llply(1:nBlocks, function(indB) process.block(inp[bl.start[indB]:bl.end[indB]], info))
    ## collect <- function(vname)
    ##     {
    ##         valid <- Filter(function(ind) !is.null(blocks[[ind]][[vname]]),1:length(blocks))
    ##         ldply(valid,function(ind) mutate(blocks[[ind]][[vname]],block=ind))
    ##     }
    collect <- function(vname) {
        # Merge the data from all the different blocks
        out <- suppressWarnings(try(
            map(blocks, vname) %>%
                compact %>%
                map_df(identity, .id = "block"),
            TRUE
        ))
        if (is(out, "try-error")) {
            sprintf("Failed to merge %s", vname) %>% warning
            # Merging has failed, return as list
            map(blocks, vname)
        } else {
            out
        }
    }
    vars <- c("raw", "msg", "sacc", "fix", "blinks", "info")
    # Collect all the data across blocks
    out <- map(vars, collect) %>% setNames(vars)

    out$info <- info
    out
}


process.block.header <- function(blk) {
    endh <- str_detect(blk, '^SAMPLES') %>% which
    has.samples <- TRUE
    # If raw data is missing, then no SAMPLES line
    if (length(endh) != 1) {
        endh <- str_detect(blk, '^EVENTS') %>% which
        has.samples <- FALSE
    }
    hd <- blk[1:endh]
    # Parse the EVENTS line
    ev <- str_select(hd, "^EVENTS")
    regex.num <- "([-+]?[0-9]*\\.?[0-9]+)"
    srate <- str_match(ev, paste0("RATE\t", regex.num))[, 2] %>% as.numeric
    tracking <- str_match(ev, "TRACKING\t(\\w+)")[, 2]
    filter <- str_match(ev, "FILTER\t(\\d)")[, 2] %>% as.numeric
    events <- list(
        left = str_detect(ev, fixed("LEFT")),
        right = str_detect(ev, fixed("RIGHT")),
        res = str_detect(ev, fixed(" RES ")),
        tracking = tracking,
        srate = srate,
        filter = filter
    )

    if (!has.samples){
        samples <- NULL
    } else {
        # Now do the same thing for the SAMPLES line
        sm <- str_select(hd, "^SAMPLES")

        srate <- str_match(sm, paste0("RATE\t", regex.num))[, 2] %>% as.numeric
        tracking <- str_match(sm, "TRACKING\t(\\w+)")[, 2]
        filter <- str_match(sm, "FILTER\t(\\d)")[, 2] %>% as.numeric

        samples <- list(
            left = str_detect(sm, fixed("LEFT")),
            right = str_detect(sm, fixed("RIGHT")),
            res = str_detect(ev, fixed(" RES ")),
            vel = str_detect(ev, fixed(" VEL ")),
            tracking = tracking,
            srate = srate,
            filter = filter
        )
    }
    list(events = events, samples = samples, the.rest = blk[-(1:endh)])
}


# Turn a list of strings with tab-separated field into a data.frame
tsv2df <- function(dat, coltypes) {
    if (length(dat) == 1) {
        dat <- paste0(dat, "\n")
    } else {
        dat <- paste0(dat, collapse = "\n")
    }
    out <- read_tsv(dat, col_names = FALSE, col_types = paste0(coltypes, collapse = ""))
    ##        if (!(is.null(attr(suppressWarnings(out), "problems")))) browser()
    out
}


parse.saccades <- function(evt, events) {
    # Focus only on EFIX events, they contain all the info
    esac <- str_select(evt, "^ESAC") %>%
        str_replace("ESACC\\s+(R|L)", "\\1\t") %>%
        str_replace_all("\t\\s+", "\t")

    # Missing data
    esac <- str_replace_all(esac, "\\s\\.", "\tNA")

    df <- str_split(esac, "\n") %>%
        ldply(function(v) { str_split(v, "\\t")[[1]] })
    # Get a data.frame
    # ESACC  <eye>  <stime>  <etime>  <dur> <sxp>  <syp>  <exp>  <eyp>  <ampl> <pv> 
    cols <- c("eye", "stime", "etime", "dur", "sxp", "syp", "exp", "eyp", "ampl", "pv", "xr", "yr")
    if (ncol(df) == 10) {
        names(df) <- cols[1:10]
    } else if (ncol(df) == 12) {
        names(df) <- cols
    }

    dfc <- suppressWarnings(llply(as.list(df)[-1], as.numeric) %>% as.data.frame)
    dfc$eye <- df$eye
    dfc
}


parse.blinks <- function(evt, events) {
    eblk <- str_select(evt, "^EBLINK") %>%
        str_replace("EBLINK\\s+(R|L)", "\\1\t") %>%
        str_replace_all("\t\\s+", "\t")

    # Get a data.frame
    #eblk <- eblk %>% tsv2df
    df <- str_split(eblk, "\n") %>%
        ldply(function(v) { str_split(v, "\\t")[[1]] })
    names(df) <- c("eye", "stime", "etime", "dur")
    dfc <- suppressWarnings(llply(as.list(df)[-1], as.numeric) %>% as.data.frame)
    dfc$eye <- df$eye
    dfc
}


parse.fixations <- function(evt, events) {
    # Focus only on EFIX events, they contain all the info
    efix <- str_select(evt, "^EFIX") %>%
        str_replace("EFIX\\s+(R|L)", "\\1\t") %>%
        str_replace_all("\t\\s+", "\t")

    # Get a data.frame
    #efix <- efix %>% tsv2df
    df <- str_split(efix, "\n") %>%
        ldply(function(v) { str_split(v, "\\t")[[1]] })

    cols <- c("eye", "stime", "etime", "dur", "axp", "ayp", "aps", "xr", "yr")
    if (ncol(df) == 7) {
        names(df) <- cols[1:7]
    } else if (ncol(df) == 9) {
        names(df) <- cols
    }
    dfc <- suppressWarnings(llply(as.list(df)[-1], as.numeric) %>% as.data.frame)
    dfc$eye <- df$eye
    dfc
}


# evt is raw text, events is a structure with meta-data from the START field
process.events <- function(evt, events) {
    # Messages
    if (any(str_detect(evt, "^MSG"))) {
        msg <- str_select(evt, "^MSG") %>%
            str_sub(start = 5) %>%
            str_match("(\\d+)\\s(.*)")
        msg <- data.frame(time = as.numeric(msg[, 2]), text = msg[, 3])
    } else {
        msg <- c()
    }

    fix <- if (str_detect(evt, "^EFIX") %>% any) parse.fixations(evt, events) else NULL
    sacc <- if (str_detect(evt, "^ESAC") %>% any) parse.saccades(evt, events) else NULL
    blinks <- if (str_detect(evt, "^SBLI") %>% any) parse.blinks(evt, events) else NULL
    list(fix = fix, sacc = sacc, msg = msg, blinks = blinks)
}


# A block is whatever one finds between a START and an END event
process.block <- function(blk, info) {
    hd <- process.block.header(blk)
    blk <- hd$the.rest
    if (is.na(info$velocity)) {
        # if no raw data
        raw <- NULL
        which.raw <- rep(FALSE, length(blk))
    } else {
        colinfo <- coln.raw(info)

        raw.colnames <- colinfo$names
        raw.coltypes <- colinfo$types

        # Get the raw data (lines beginning with a number)
        which.raw <- str_detect(blk, '^\\d')
        raw <- blk[which.raw] %>% str_select('^\\d') # %>% str_replace(fixed("\t..."),"")
        #        raw <- str_replace(raw,"\\.+$","")

        # Filter out all the lines where eye position is missing, they're pointless
        # and stored in an inconsistent manner
        iscrap <- str_detect(raw, "\\s+\\.\\s+\\.\\s+")
        crap <- raw[iscrap]
        raw <- raw[!iscrap]
        if (length(raw) > 0) {
            # If we have some data left, turn into data.frame
            raw <- tsv2df(raw, raw.coltypes)
            if (ncol(raw) == length(raw.colnames)) {
                names(raw) <- raw.colnames
            } else {
                warning(paste(
                    "Unknown columns in raw data.",
                    "Assuming first one is time, please check the others"
                ))
                #names(raw)[1:length(raw.colnames)] <- raw.colnames
                names(raw)[1] <- "time"
            }
            nCol <- ncol(raw)
            if (any(iscrap)) {
                crapmat <- matrix(NA, length(crap), nCol)
                crapmat[, 1] <- as.numeric(str_match(crap, "^(\\d+)")[, 1])
                crapmat <- as.data.frame(crapmat)
                names(crapmat) <- names(raw)
                raw <- rbind(raw, crapmat)
                raw <- raw[order(raw$time), ]
            }
        } else {
            warning("All data are missing in current block")
            raw <- NULL
        }
    }
    # The events (lines not beginning with a number)
    evt <- blk[!which.raw]
    res <- process.events(evt, hd$events)
    res$raw <- raw
    res$sampling.rate <- hd$events$srate
    res$left.eye <- hd$events$left
    res$right.eye <- hd$events$right
    res
}


# Gets useful metadata about the tracker setup & settings from the file
getInfo <- function(inp) {
    nonsample <- inp[str_detect(inp, "^\\D")]
    header <- nonsample[str_detect(nonsample, "^\\*\\*")]
    info <- data.frame(
        date = NA, model = NA, version = NA, sample.rate = NA, cr = NA,
        left = NA, right = NA, mono = NA, screen.x = NA, screen.y = NA,
        filter.level = NA, velocity = NA, resolution = NA, htarg = NA, input = NA
    )
    # Get date/time of recording from file
    info$date <- as.POSIXct(from.header(header, "DATE"), format = "%a %b %d %H:%M:%S %Y")
    # Get tracker model/version info
    version_info <- get.model(header)
    info$model <- version_info[1]
    info$version <- version_info[2]
    # Get display size from file
    display_xy <- nonsample[grepl("DISPLAY_COORDS", nonsample)]
    display_xy <- gsub('.* DISPLAY_COORDS\\s+(.*)', '\\1', display_xy)
    display_xy <- as.numeric(unlist(strsplit(display_xy, split = '\\s+')))
    info$screen.x <- display_xy[3] - display_xy[1] + 1
    info$screen.y <- display_xy[4] - display_xy[2] + 1
    # Find the last config line in file
    config <- nonsample[grepl("^EVENTS|^SAMPLES", nonsample)]
    config <- config[length(config)]
    if (length(config) > 0) {
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
        # Even in remote setups, the target information may not be recorded so we make 
        # sure it actually is (e.g. binoRemote250.asc)
        info$htarg <- FALSE
        if (grepl("\tHTARG", config)) {
            # Normally the htarg stuff is just twelve dots in a row, but in case
            # there are errors we need the following regexp.
            pat <- paste0(
                "(M|\\.)(A|\\.)(N|\\.)(C|\\.)(F|\\.)(T|\\.)(B|\\.)",
                "(L|\\.)(R|\\.)(T|\\.)(B|\\.)(L|\\.)(R|\\.)"
            )
            info$htarg <- str_detect(inp, pat) %>% any
        }
        info$input <- grepl("\tINPUT", config)
        info$left <- grepl("\tLEFT", config)
        info$right <- grepl("\tRIGHT", config)
        info$mono <- !(info$right & info$left)
    }
    info
}


# Extracts the value of a given field from the ASC header
from.header <- function(header, field) {
    s <- header[grepl(paste0("\\*\\* ", field), header)]
    ifelse(length(s) > 0, gsub(paste0("\\*\\* ", field, ": (.*)"), "\\1", s), NA)
}


# Gets the model and software version of the tracker based on the header contents
get.model <- function(header) {
    version_str <- from.header(header, "VERSION")
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


# Column names for the raw data
coln.raw <- function(info) {
    eyev <- c("xp", "yp", "ps")
    ctype <- rep("d", 3)
    if (info$velocity) {
        eyev <- c(eyev, "xv", "yv")
        ctype <- c(ctype, rep("d", 2))
    }
    if (info$resolution) {
        eyev <- c(eyev, "xr", "yr")
        ctype <- c(ctype, rep("d", 2))
    }
    if (!info$mono) {
        eyev <- c(paste0(eyev, "l"), paste0(eyev, "r"))
        ctype <- rep(ctype, 2)
    }

    # With corneal reflections we need an extra column
    if (info$cr) {
        eyev <- c(eyev, "cr.info")
        ctype <- c(ctype, "c")
    }

    # Three extra columns for remote set-up
    if (info$htarg) {
        eyev <- c(eyev, "tx", "ty", "td", "remote.info")
        ctype <- c(ctype, c("d", "d", "d", "c"))
    }

    list(names = c("time", eyev), types = c("i", ctype))
}
