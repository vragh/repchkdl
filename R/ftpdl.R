#' Download a File from FTP URL
#'
#' Downloads the file matching the given file name from the given FTP URL. Used
#' internally by repchkdl.
#'
#' @author Venket Raghavan
#'
#' @param burl (character) FTP URL that should be scanned.
#' @param dlfile (character) name of the file to be downloaded.
#' @param outwd (character; optional) directory where downloaded files should be
#'   saved. (Default: location obtained from getwd())
#' @param fusr (character; optional) username used to access the FTP URL.
#' @param fpwd (character; optional) password for the FTP URL.#'
#' @param foverwrite (logical; optional) controls whether the local copy of a
#'   file to be downloaded should be overwritten or not. TRUE - local copy will
#'   be overwritten; FALSE - local copy will not be overwritten, the downloaded
#'   file will be saved with the prefix "new_" instead. (Default: FALSE)
#' @param fdlretry (logical; optional) controls whether a file download should
#'   be retried if it fails. Set to TRUE to enable retries. This parameter is
#'   usually used in conjunction with the parameter redllim. (Default: FALSE)
#' @param fredllim (numeric; optional) controls how many attempts should be made
#'   to redownload a file upon download failure. This parameter is to be used in
#'   conjunction with the parameter dlretry. If dlretry is set to FALSE, setting
#'   redllim will have no effect. (Default: 1L)
#' @param verbose (logical) controls whether console messages should be
#'   displayed (e.g., messages such as "Downloading file: "). Set to FALSE to
#'   enable silent mode. (Default: TRUE)
#'
#' @return a character string ("FAILURE"/"SUCCESS") indicating whether the
#'   download was (un)successful.
#' @examples
#' \dontrun{
#' #Fetch some files with repchkdl::ftpreadlinks
#' #This URL is a test FTP server, and contains random files!
#' #This test server's password changes frequently
#' #Check the password at dlptest.com (and update) before proceeding!
#' test <- ftpreadlinks(rurl = "ftp://ftp.dlptest.com",
#' rusr = "dlpuser@dlptest.com", rpwd = "SzMf7rTE4pCrf9dV286GuNe4N")
#'
#' #Download just the first file in test as an example
#' #Output will be saved wherever getwd() is pointing to.
#' test$dlstats[1] <- ftpdl(burl = "ftp://ftp.dlptest.com",
#'                    dlfile = mylinks$flinks[i], verbose = TRUE,
#'                    fusr = "dlpuser@dlptest.com",
#'                    fpwd = "SzMf7rTE4pCrf9dV286GuNe4N")
#' }
#'
#' @export

ftpdl <- function(burl = NULL, dlfile = NULL, outwd = getwd(),
                  fusr = NULL, fpwd = NULL, foverwrite = FALSE,
                  fredllim = 1L, fdlretry = FALSE, verbose = FALSE){

  if(is.null(burl)) stop("No URL provided for download!")
  if(is.null(dlfile)) stop("No file name provided for download!")

  #Download success/failure tracker
  dlstat <- c()

  #Remove trailing / from burl and outwd if any
  burl <- stringr::str_replace(burl, "\\/$", "")
  outwd <- stringr::str_replace(outwd, "\\/$", "")
  #Create outdir in case it does not exist
  if(!base::dir.exists(outwd)) base::dir.create(outwd, recursive = TRUE)

  #Conditionally setting up usr+pwd
  #These are passed as one combined variable, mycred
  if(!is.null(fusr) && !is.null(fpwd)){
    mycred <- paste(fusr, fpwd, sep = ":")
    myopts <- parse(text = "RCurl::curlOptions(userpwd = mycred, ftp.use.epsv = FALSE, forbid.reuse = TRUE)")
  } else if( (!is.null(fusr) && is.null(fpwd)) | (is.null(fusr) && !is.null(fpwd)) ){
    stop("Username or password provided, one of the two is missing! Please check inputs and try again: \n", "Usr: ", fusr, "\nPwd: ", fpwd)
  } else{
    myopts <- parse(text = "RCurl::curlOptions(ftp.use.epsv = FALSE, forbid.reuse = TRUE)")
  }


  #Input download file
  mydlfile <- paste0(burl, "/", dlfile)

  #Outfile
  myoutfile <- paste0(outwd, "/", dlfile)

  #If outfile already exists and the user has chosen not to overwrite
  #just download the file with a prefix "new_" attached to it
  if(base::file.exists(myoutfile) & !foverwrite) {

    if(verbose) cat("File: ", dlfile, "\nexists on disk! Downloaded file will be saved with prefix new_!\n")

    myoutfile <- paste0(outwd, "/", "new_", dlfile)

  }



  if(verbose) cat("Downloading: ", mydlfile, "\n")



  #DOWNLOAD CODE
  #First attempt
  mydlbin <- base::tryCatch(
    RCurl::getBinaryURL(mydlfile, .opts = eval(myopts)),
    error = function(e) e
  ) #Downloading binary data

  #Download retries
  if(fdlretry){

    if(base::inherits(mydlbin, "error") && fredllim > 1){

      if(verbose) cat("It appears the first download attempt failed. Download retry option is enabled. Will attempt a re-download!\n")

      myruns <- 1

      while(base::inherits(mydlbin, "error") && myruns <= fredllim){

        if(verbose) cat("Download retry attempt ", myruns, "\n")

        mydlbin <- base::tryCatch(
          RCurl::getBinaryURL(mydlfile, .opts = eval(myopts)),
          error = function(e) e
        ) #Downloading binary data

        myruns <- myruns + 1

      }

    }
  }


  #If tryCatch returned an error, i.e., file did not download properly
  if(base::inherits(mydlbin, "error")){

    if(verbose) cat("Failed to download: ", mydlfile, "\n")

    dlstat <- "FAILURE"

    #next #Will continue with next file

  } else{

    base::writeBin(mydlbin, myoutfile) #Writing binary data to file

    if(verbose) cat("Done!\n")

    dlstat <- "SUCCESS"
  }

  return(dlstat)

}
