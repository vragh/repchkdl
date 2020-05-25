#' Automatically Download Newly Added Files from a FTP URL
#'
#' Given a FTP URL hosting a collection of files, repchkdl will repeatedly check
#' if new files have been added to the URL (since the first time it checks the
#' URL). repchkdl accepts optional login credentials (username and password) if
#' necessary. If new files are available, repchkdl will indicate the files and
#' offer to download them to a directory of choice. What constitutes new files
#' can be constrained on the basis of regular expressions and file sizes (lower
#' as well as upper bounds) supplied to the function as optional parameters. The
#' URL polling frequency, the total number of scan repetitions, re-download
#' attempts (upon failure), and whether files should actually be downloaded (or
#' if repchkdl should simulate the downloads) are other available options. Silent
#' mode operation is also possible.
#'
#' repchkdl makes use of two functions internally: ftpreadlinks and ftpdl. The
#' former fetches the names of files at a URL, and the latter downloads the
#' files. ftpdl is basically a wrapper around Rcurl::getBinaryURL(). These
#' functions are also exported and can be called by the user.
#'
#' Notes:
#'
#' Granular control over which files should be downloaded is not available
#' at this time (i.e., if a more than one new file is available at the URL at the
#' time of checking, all available files files will be downloaded together).
#'
#' ftp://, https://, and http:// prefixed URLs are valid.
#'
#' At present files are downloaded using Curl (via RCurl).
#'
#' As of v.0.2.0, repchkdl cannot distinguish between directories/folders and
#' files (its use of RCurl::getBinaryURL() precludes this). If directories are
#' present alongside files, repchkdl will attempt to download them (and fail),
#' and this will result in that particular download's entry being recorded as a
#'"FAILURE" in the final output data.frame.
#'
#' @author Venket Raghavan
#'
#' @param url (character) FTP URL that should be scanned.
#' @param outdir (character; optional) directory where downloaded files should be
#'  saved. (Default: location obtained from getwd())
#' @param usr (character; optional) username used to access the FTP URL.
#' @param pwd (character; optional) password for the FTP URL.
#' @param sizelb (numeric; optional) minimum file size (in bytes). Only files
#'  equal to or above this threshold will be downloaded.
#' @param sizeub (numeric; optional) maximum file size (in bytes). Files equal to
#'  or above this threshold will NOT be downloaded.
#' @param regex (character; optional) regular expression to indicate what file
#'  additions at the URL should be considered for download. The regex format
#'  follows stringr conventions.
#' @param overwrite (logical; optional) controls whether the local copy of a file
#'  to be downloaded should be overwritten or not. TRUE - local copy will be
#'  overwritten; FALSE - local copy will not be overwritten, the downloaded file
#'  will be saved with the prefix "new_" instead. (Default: FALSE)
#' @param dlretry (logical; optional) controls whether a file download should be
#'  retried if it fails. Set to TRUE to enable retries. This parameter is
#'  usually used in conjunction with the parameter redllim. (Default: FALSE)
#' @param redllim (numeric; optional) controls how many attempts should be made
#'  to redownload a file upon download failure. This parameter is to be used in
#'  conjunction with the parameter dlretry. If dlretry is set to FALSE, setting
#'  redllim will have no effect. (Default: 1L)
#' @param autoscan (logical; optional) controls whether successive scanning
#'  iterations should be performed automatically or upon user prompt. Set to
#'  FALSE to approve successive scan iterations manually. (Default: TRUE)
#' @param autodl (logical; optional) controls whether repchkdl should download
#'  files automatically or if the user should be prompted. Set to TRUE to bypass
#'  user prompt. (Default: FALSE)
#' @param wait (numeric; optional) URL polling interval in seconds. Provide a
#'  smaller number to scan the URL more frequently and vice versa. (Default: 4)
#' @param enabledl (logical; optional) controls whether the files should actually
#'  be downloaded or only simulated (i.e., names of files will printed to
#'  terminal as having been downloaded). Set to TRUE to enable. (Default: FALSE)
#' @param scanlim (numeric; mandatory) sets the maximum number of times repchkdl
#'  should execute before exiting. It is strongly recommnended to set a finite
#'  numerical value (e.g., 10) for this parameter! If unset, repchkdl will run
#'  indefinitely, and will have to be exited forcibly (e.g., Ctrl + C)!
#'  (Default: NULL)
#' @param firstrundl (logical, NULL; optional) controls whether repchkdl should
#'  automatically download all matching/available files upon first invocation.
#'  TRUE - first run files will be downloaded; FALSE - first run files will be
#'  skipped; NULL - user will be prompted for a decision. This is useful, for
#'  example, if the current set of files at the URL are available locally, and
#'  repchkdl is only needed to check for new additions. (Default: NULL)
#' @param verbosity (logical) controls whether console messages should be
#'  displayed (e.g., messages such as "Downloading file: "). Set to FALSE to
#'  enable silent mode. (Default: TRUE)
#'
#' @return a data.frame containing the names of the files, their sizes (in
#'  bytes), and a column indicating download success/failure.
#'
#'
#' @examples
#' \dontrun{
#' #Working example w/ username+password - change the regex to look for other files;
#' #run with scanlim = 1 and no regex to get a list of all files
#' #This URL is a test FTP server, and contains random files!
#' #This test server's password changes frequently
#' #Check the password at dlptest.com (and update) before proceeding!
#' #(Hence enabledl = FALSE; but the files should be safe to download in theory)
#' test <- repchkdl(url = "ftp://ftp.dlptest.com", usr = "dlpuser@dlptest.com",
#'                  pwd = "SzMf7rTE4pCrf9dV286GuNe4N", regex = ".*.txt",
#'                  enabledl = FALSE, verbosity = TRUE, scanlim = 1)
#' }
#' \dontrun{
#' #This one will result in a "FAILURE" being written to dlstats
#' #since it will try to download a directory
#' #This also illustrated the user of the dlretry and redllim parameters
#' test <- repchkdl(url ="ftp://cran.r-project.org/pub/R/", scanlim = 1,
#'                  enabledl = T, regex = "^html$", dlretry = T, redllim = 2)
#' }
#'
#' @export

repchkdl <- function(url = NULL, outdir = getwd(),
                     regex = NULL, usr = NULL, pwd = NULL,
                     sizelb = NULL, sizeub = NULL, overwrite = FALSE,
                     dlretry = FALSE, redllim = 1L, firstrundl = NULL,
                     autodl = FALSE, enabledl = FALSE, scanlim = NULL,
                     autoscan = TRUE, wait = 4, verbosity = TRUE){

  #Break if no URL is provided
  if(is.na(url)) stop("No URL provided!")


  #Counter keeping track of main loop
  my_n <- 0

  #Data.frame of all files and success/failure stats
  all_dls <- data.frame(flinks = c(), fsizes = c(), dlstats = c(), stringsAsFactors = FALSE)


  #Set up curlinks and prevlinks
  curlinks <- ftpreadlinks(rurl = url, rusr = usr, rpwd = pwd,
                           rsizelb = sizelb, rsizeub = sizeub,
                           rregex = regex, verbose = verbosity)
  prevlinks <- curlinks


  #Optional download upon first run


  if(my_n == 0){

    if(nrow(curlinks) == 0 | is.null(curlinks)) {

      if(verbosity) cat("First run: no files available to download! Will continue scanning!\n")

    } else{

      if(is.null(firstrundl)){

        if(verbosity) cat("This is the first run of repchkdl. The following files are available for download:", curlinks$flinks, sep = "\n")
        firstrundl <- readline("This is the first run of repchkdl. Download all available, matching files? (y/n): ")
        firstrundl <- ifelse(firstrundl == "y", TRUE, FALSE)

      }

      #Download all first run files if user accepts
      if(firstrundl){

        for(i in 1:nrow(curlinks)){

          if(enabledl){

            curlinks$dlstats[i] <- ftpdl(burl = url, dlfile = curlinks$flinks[i],
                                         fusr = usr, fpwd = pwd, outwd = outdir,
                                         fdlretry = dlretry, foverwrite = overwrite,
                                         fredllim = redllim, verbose = verbosity)

          } else{

            if(verbosity) cat("Downloading: ", curlinks$flinks[i], "\nDone!\n")
            curlinks$dlstats[i] <- "SUCCESS"

          }

        }

        #Adding names of successfully downloaded files from first run (if firstrundl enabled) to tracker
        all_dls <- base::rbind(all_dls, curlinks)

      }

    }

  }



  #Setting main checking loop control variables
  #rescan_yn <- autoscan
  if(autoscan) rescan_yn <- "y"
  if(!autoscan) rescan_yn <- "n"

  #----
  #BEGIN MAIN LOOP
  while(rescan_yn == "y"){

    #mylinks <- ftpreadlinks(rurl = "ftp://ftp.dlptest.com",
    #rusr = "dlpuser@dlptest.com", rpwd = "SzMf7rTE4pCrf9dV286GuNe4N",
    #rsizeub = 1000, rregex = ".*.txt")
    prevlinks <- curlinks



    #Update curlinks
    curlinks <- ftpreadlinks(rurl = url, rusr = usr, rpwd = pwd,
                             rsizelb = sizelb, rsizeub = sizeub,
                             rregex = regex, verbose = verbosity)

    #Getting subset of newly added files
    #STEP THAT ENABLES CHECKING OF NEW FILES
    #BASICALLY SETDIFF but with data.table's function
    #which enables diff of two data.frames!
    #newlinks <- data.frame(data.table::setDT(curlinks)[!prevlinks, on = "flinks"], stringsAsFactors = FALSE)
    newlinks <- curlinks[!(curlinks$flinks %in% prevlinks$flinks), ]
    #t2[!(t2$flinks %in% mylinks$flinks), ]

    #BEGIN DOWNLOAD SECTION
    #Downloading new files if any are available
    #Conditional upon user requirements
    if(nrow(newlinks) > 0){

      if(verbosity) cat("Following new files found at provided URL:", newlinks$flinks, sep = "\n")

      if(!autodl){
        dl_now <- readline("Download these files? (y/n): ")
      } else{
        dl_now <- "y"
      }

      if(dl_now == "y"){

        #Main download loop
        for(i in 1:nrow(newlinks)){

          if(enabledl){

            newlinks$dlstats[i] <- ftpdl(burl = url, dlfile = newlinks$flinks[i],
                                         fusr = usr, fpwd = pwd, outwd = outdir,
                                         fdlretry = dlretry, foverwrite = overwrite,
                                         fredllim = redllim, verbose = verbosity)

          } else{

            #Simulating downloads only!
            if(verbosity) cat("Downloading: ", newlinks$flinks[i], "\nDone!")
            newlinks$dlstats[i] <- "SUCCESS"

          }

        }

      }

    }

    #If no new files are found, announce so (if verbosity is enabled)
    if(nrow(newlinks) == 0){

      if(verbosity) cat(paste0("No new files!\n"))

    }
    #END DOWNLOAD SECTION

    if(!autoscan){

      #Ask user if they would like to scan again
      rescan_yn <- readline("Scan again? (y/n): ")

      if(rescan_yn == "n"){

        if(verbosity) cat("Finished. Exiting! Here is the list of files downloaded so far: \n")

      }

    }

    #Adding names of successfully downloaded files to tracker
    #all_dls <- base::rbind(all_dls, newlinks)


    #Update prevlinks within while loop for next run
    prevlinks <- curlinks

    #If scanlim is set, loop will quit here
    if(!is.null(scanlim) & my_n == scanlim){
      break
    }

    #Announce rescan count
    if(verbosity) cat(paste0("Rescanning for the ",
                             (my_n + 1),
                             ifelse((my_n + 1) == 1, "st", ifelse((my_n + 1) == 2, "nd", ifelse((my_n + 1) == 3, "rd", "th"))),
                             " time.\n"))

    #Update my_n
    my_n <- my_n + 1

    Sys.sleep(wait)




  }
  #END MAIN LOOP
  #----


  if(verbosity) cat("Finished. Exiting! Here is the list of files downloaded so far:\n")
  if(verbosity) print(all_dls)

  return(all_dls)
  #return(newlinks)

}
