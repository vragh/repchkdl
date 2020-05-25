#' Get set of file names and sizes from FTP URL
#'
#' Accesses a FTP URL and returns a data.frame of files and their sizes in
#' bytes. The set of files returned can be filtered by file size and regular
#' expressions. Used internally by repchkdl.
#'
#' @author Venket Raghavan
#'
#' @param rurl (character) FTP URL that should be scanned.
#' @param rusr (character; optional) username used to access the FTP URL.
#' @param rpwd (character; optional) password for the FTP URL.
#' @param rsizelb (numeric; optional) minimum file size (in bytes). Only files
#'   equal to or above this threshold will be downloaded.
#' @param rsizeub (numeric; optional) maximum file size (in bytes). Files equal
#'   to or above this threshold will NOT be downloaded.
#' @param rregex (character; optional) regular expression to indicate what file
#'   additions at the URL should be considered for download. The regex format
#'   follows stringr conventions.
#' @param verbose (logical) controls whether console messages should be
#'   displayed (e.g., messages such as "Downloading file: "). Set to FALSE to
#'   enable silent mode. (Default: TRUE)
#'
#' @return a data.frame containing the names of the files and their sizes (in
#'   bytes) matching the optional constraints (e.g., minimum/maximum file size
#'   thresholds) available at the FTP URL for download.
#'
#' @examples
#' \dontrun{
#' #This URL is a test FTP server, and contains random files!
#' #This test server's password changes frequently
#' #Check the password at dlptest.com (and update) before proceeding!
#' #If this example returns a zero length data.frame, rerun without a regex
#' #It is possible that this test server might not have any .txt files at
#' #the time of running this example!
#' test <- ftpreadlinks(rurl = "ftp://ftp.dlptest.com",
#'                      rusr = "dlpuser@dlptest.com",
#'                      rpwd = "SzMf7rTE4pCrf9dV286GuNe4N",
#'                      rregex = ".*.txt")
#' }
#'
#' @export

ftpreadlinks <- function(rurl = NULL, rusr = NULL, rpwd = NULL,
                         rsizelb = NULL, rsizeub = NULL,
                         rregex = NULL, verbose = FALSE){

  #----
  #For testing
  #url <- "ftp://ftp.ncdc.noaa.gov/pub/data/airsea/"

  #url <- "ftp://ftp.dlptest.com"
  #usr <- "dlpuser@dlptest.com"
  #pwd <- "SzMf7rTE4pCrf9dV286GuNe4N" #This password changes very frequently, so check before using it
  #sizelim <- 1000 #in bytes
  #regex <- ".*.txt"

  #url <- "ftp://ftp.swfwmd.state.fl.us/pub/usf/"
  #usr <- "anonymous"
  #pwd <- "abc@def.com"
  #sizelim <- 770000
  #regex <- ".*.jpg"
  #----

  if(is.null(rurl)) stop("Please supply a URL!")

  #Remove http:// and https:// prefixes if any
  if(stringr::str_detect(rurl, "^https://|http://")) rurl <- stringr::str_replace(rurl, "^http[s]*://", "")


  #Conditionally passing usr+pwd (if any) to getURL and getBinaryURL
  if(!is.null(rusr) && !is.null(rpwd)){
    mycred <- paste(rusr, rpwd, sep = ":")
    myopts <- parse(text = "RCurl::curlOptions(userpwd = mycred, ftp.use.epsv = FALSE, forbid.reuse = TRUE)")
  } else if( (!is.null(rusr) && is.null(rpwd)) | (is.null(rusr) && !is.null(rpwd)) ){
    stop("Username or password provided, one of the two is missing! Please check inputs and try again: \n", "Usr: ", rusr, "\nPwd: ", rpwd)
  } else{
    myopts <- parse(text = "RCurl::curlOptions(ftp.use.epsv = FALSE, forbid.reuse = TRUE)")
  }


  #Getting data from the URL
  curlinks <- RCurl::getURL(rurl, .opts = eval(myopts))


  #Setting up curlinks as a dataframe with sizes and filenames available as individual columns
  curlinks <- data.frame(fullstr = unlist(stringr::str_split(curlinks, "\n")), stringsAsFactors = FALSE)
  curlinks$flinks <- stringr::str_extract(curlinks$fullstr, "[\\w\\.\\_\\-]+(?=$)")
  curlinks$fsizes <- as.numeric(stringr::str_extract(curlinks$fullstr, "[0-9]+(?=\\s[A-Z][a-z]{2})"))
  curlinks <- curlinks[, 2:3] #Removing the fullstr column as it isn't really necessary


  #Filtering out current and parent directories (. and ..)
  curlinks <- curlinks[!stringr::str_detect(curlinks$flinks, "^\\.$|^\\..$"), ]
  #Removing any NA lines that might have shown up
  curlinks <- curlinks[stats::complete.cases(curlinks), ]


  #Filtering the matches by size and regex
  if(!is.null(rsizelb)) curlinks <- curlinks[curlinks$fsizes >= rsizelb, ]
  if(!is.null(rsizeub)) curlinks <- curlinks[curlinks$fsizes < rsizeub, ]
  if(!is.null(rregex)) curlinks <- curlinks[stringr::str_detect(curlinks$flinks, rregex), ]


  if(verbose) cat("Finished fetching files and details from ", rurl, "\n")

  #Returning curlinks to parent env
  return(curlinks)

}
