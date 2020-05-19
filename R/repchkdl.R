#' Continually Check for and Download Newly Added Files from Given URL Matching Optional Regex
#'
#' This function downloads data from the given URL and keeps checking for new
#' files at the URL thereafter. Should new files become available, the function
#' offers to download them. A regular expression can be supplied to the function
#' to indicate what should be considered valid "new" files; e.g, "txt" would
#' check and alert the user/download the files if and only if a file matching
#' the regex "txt" becomes available at the URL. Note: repchkdl will have to be
#' forcibly exited if autoscan = "y" is set, as there is no way to break the recursion
#' otherwise. This CAN cause the latest set of files to not be downloaded!
#'
#' @param inpurl (character) URL that should be scanned
#' @param inpwd (character; optional) directory where downloaded files should be saved (default location supplied by getwd())
#' @param inpregex (character; optional) regular expression (default NA) to indicate what file additions at the URL should be considered for download
#' @param autoscan (y/n) set to "y" (default) to have repchkdl rescan automatically
#' @param autodl (y/n) set to "y" (default) to have repchkdl download new files automatically
#' @param inpwait (double) time in seconds (default 2 seconds) after which repchkdl should poll the URL again
#' @param enabledl (y/n) if set to "n" (default "y") repchkdl will SIMULATE downloads (i.e., nothing will be downloaded even if the terminal prints "Downloading file: ")
#' @param scanlim (integer; optional) sets the number of times repchkdl should run before exiting
#' @param firstrundl (y/n) if set to "y" (default) repchkdl will automatically download all matching/available files upon first invocation. Setting to "n" will prompt the user. The user can select to skip downloading the files upon first run; this is useful, for example, if the current set of files at the URL have been downloaded already, and repchkdl is only needed to check for additions hereafter.
#'
#' @return a vector containing the names of all files that have been downloaded
#'
#' @export
#'
#' @examples \dontrun{test <- repchkdl(inpurl = "ftp://speedtest.tele2.net/upload/",
#' inpregex = ".txt", autoscan = "y", autodl = "y",
#' enabledl = "n", scanlim = 4, inpwait = 2)}

repchkdl <- function(inpurl = NA, inpwd = getwd(), inpregex = NA, autoscan = "y", autodl = "y", inpwait = 2, enabledl = "y", scanlim = NA, firstrundl = "y"){

  #FUNCTION BEGIN ----------------------------------------------------------------


  #Break if no URL is provided
  if(is.na(inpurl)){
    stop("No URL provided. Exiting.\n")
  }




  #Copying stuff over to local variables
  myurl <- inpurl
  mywd <- inpwd
  myregex <- inpregex





  #Some conditional parsing
  #Also sets the current and previous versions
  #of files available at the URL at the time of
  #starting this script

  if(stringr::str_detect(myurl, "^ftp")){

    #For ftp://*

    curlinks <- RCurl::getURL(myurl, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    curlinks <- unlist(stringr::str_split(curlinks, "\n"))
    prevlinks <- curlinks

  } else if(stringr::str_detect(myurl, "^https")){

    #For https://ftp.*

    urldat <- RCurl::getURL(myurl)
    curlinks <- XML::getHTMLLinks(urldat)
    prevlinks <- curlinks

  }




  #Setting loop control variables for main loop
  rescan_yn <- autoscan
  do_dl <- autodl
  my_n <- 0 #This is just a counter to keep track of how many times the loop has been repeated

  #all_dlfiles will just accumulate all the files downloaded by the user, and return them at the end
  all_dlfiles <- c()





  #If it is the first run,
  #offer to download all available files
  if(my_n == 0){


    if(firstrundl == "n"){
      firstrundl <- readline("This appears to be the first time this script is being launch. Should all requested files be downloaded (y/n): ")
    }

    if(firstrundl == "y"){

      if(length(curlinks) == 0){
        cat("No files available for download! repchkdl will continue scanning.\n")
      } else{
        if(is.na(myregex)){

          for(j in 1:length(curlinks)){

            baseurl <- myurl
            dl_file <- curlinks[j]

            cat(paste0("Downloading: ", dl_file, "\n"))

            if(enabledl == "y"){
              utils::download.file(url = paste0(baseurl, dl_file), destfile = paste0(mywd, "/", dl_file))
            }


            all_dlfiles <- c(all_dlfiles, dl_file)

          }

        } else{

          for(j in 1:length(curlinks[stringr::str_detect(curlinks, myregex)])){

            baseurl <- myurl
            dl_file <- curlinks[stringr::str_detect(curlinks, myregex)][j]

            cat(paste0("Downloading: ", dl_file, "\n"))

            if(enabledl == "y"){
              utils::download.file(url = paste0(baseurl, dl_file), destfile = paste0(mywd, "/", dl_file))
            }


            cat("Done!\n")

            all_dlfiles <- c(all_dlfiles, dl_file)

          }
        }

      }

    }

  }




  #MAIN WHILE LOOP ----------------------------------------------------------------
  #While user says they want to rescan
  while(rescan_yn == "y"){



    #This is the same if else clause from earlier
    #but modified.
    #The one outside the while loop ensures that prevlinks is set
    #This one ensures that curlinks keeps getting updated as long
    #as the while loop is active

    if(stringr::str_detect(myurl, "^ftp")){
      #For ftp://*
      curlinks <- RCurl::getURL(myurl, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      curlinks <- unlist(stringr::str_split(curlinks, "\n"))
      #prevlinks <- curlinks

    } else if(stringr::str_detect(myurl, "^https")){

      #For https://ftp.*
      urldat <- RCurl::getURL(myurl)
      curlinks <- XML::getHTMLLinks(urldat)
      #prevlinks <- curlinks

    }




    #Setdiff checks what is new in curlinks in comparison to prevlinks
    #Does not care what is already in prevlinks
    new_files <- setdiff(curlinks, prevlinks)



    #Check if new files have been added to the ftp server,
    #if yes, download them to directory specified by user (getwd() by default)
    #if not, print a message stating nothing new has been added
    if(length(new_files) > 0){

      #If regex is enabled
      if(!is.na(myregex) & length(new_files[stringr::str_detect(new_files, myregex)]) != 0){

        cat("Found following new files matching the regex:", new_files[stringr::str_detect(new_files, myregex)], sep = "\n")

        #Ask user if the files must be downloaded

        if(do_dl != "y"){
          dl_now <- readline("Download these files? (y/n): ")
        } else{
          dl_now <- do_dl
        }

        if(dl_now == "y"){

          for(j in 1:length(new_files[stringr::str_detect(new_files, myregex)])){
            baseurl <- myurl
            dl_file <- new_files[stringr::str_detect(new_files, myregex)][j]

            cat(paste0("Downloading: ", dl_file, "\n"))

            if(enabledl == "y"){
              utils::download.file(url = paste0(baseurl, dl_file), destfile = paste0(mywd, "/", dl_file))
            }

            cat("Done!\n")

            all_dlfiles <- c(all_dlfiles, dl_file)

          }

        }

      } else if (is.na(myregex)){ #No regex provided; the if-for construct below is identical to the one above

        cat("Found new files:", new_files, sep = "\n")

        #Ask user if the files must be downloaded

        if(do_dl != "y"){
          dl_now <- readline("Download these files? (y/n): ")
        } else{
          dl_now <- do_dl
        }

        if(dl_now == "y"){

          for(j in 1:length(new_files)){
            baseurl <- myurl
            dl_file <- new_files[j]

            cat(paste0("Downloading: ", dl_file, "\n"))

            if(enabledl == "y"){
              utils::download.file(url = paste0(baseurl, dl_file), destfile = paste0(mywd, "/", dl_file))
            }

            cat("Done!\n")

            all_dlfiles <- c(all_dlfiles, dl_file)

          }

        }

      }

    } else{

      cat(paste0("No new files, rescanning for the ",
                 (my_n + 1),
                 ifelse((my_n + 1) == 1, "st", ifelse((my_n + 1) == 2, "nd", ifelse((my_n + 1) == 3, "rd", "th"))),
                 " time.\n"))

    }





    if(autoscan == "n"){
      #Ask user if they would like to scan again
      rescan_yn <- readline("Scan again? (y/n): ")

      if(rescan_yn == "n"){
        cat("Finished. Exiting! Here is the list of files downloaded so far: \n")
      }
    }



    #Update prevlinks
    prevlinks <- curlinks

    #If scanlim is set, loop will quit here
    if(!is.na(scanlim) & my_n == scanlim){
      break
    }

    #Update counter
    my_n <- my_n + 1

    Sys.sleep(inpwait)


  } #End of while loop

  #MAIN WHILE LOOP ----------------------------------------------------------------





  #Prints files downloaded to terminal
  cat("Finished. Exiting! Here is the list of files downloaded so far: ", all_dlfiles, sep = "\n")

  #Returning the list of files downloaded
  return(all_dlfiles)

  #Done

  #FUNCTION END ----------------------------------------------------------------
}
