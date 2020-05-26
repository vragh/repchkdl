[![Build Status](https://travis-ci.com/vragh/repchkdl.svg?branch=master)](https://travis-ci.com/vragh/repchkdl)

# repchkdl v2.0.0
`repchkdl` is a small R package that consists of a single, eponymous function that--given a URL hosting a collection of files on a FTP server--will repeatedly check if new files have been added to the URL (since the first time it checks the URL). If new files are available, repchkdl will indicate the files and offers to download them. An optional regular expression pattern can be specified to constrain what files repchkdl should consider as valid new additions (based on the file name(s)). repchkdl saves the downloaded files to the default working directory unless an alternative path is specified. When invoked for the first time, `repchkdl` will offer to download all available files (matching constraints if any are supplied). Options to control whether files are actually downloaded or not, the frequency at which the URL should be polled, and manual control over scanning and downloading (among other things) are available (please refer to the package documentation for details). repchkdl makes use of two functions internally: `ftpreadlinks` and `ftpdl`. The former fetches the names of files at a URL, and the latter downloads the files. `ftpdl` is basically a wrapper around `Rcurl::getBinaryURL()`. These functions are also exported and can be called by the user.


### Example usage:
````R
test <- repchkdl(inpurl = "ftp://speedtest.tele2.net/upload/", 
                 autoscan = FALSE, autodl = FALSE, scanlim = 2L)
````
The object `test` is a `data.frame` that holds all the files encountered, their sizes (in bytes), and whether the downloads succeeded or not:
```R
flinks  fsizes  dlstats
fileA   1000    SUCCESS
fileB   1000    FAILURE
fileC   2000    FAILURE
fileD   2000    SUCCESS
````


### Notes:
* If `autoscan` is set to `TRUE`, and `scanlim` is set to `NULL`, `repchkdl` will run indefinitely! The only way to kill the script then is to forcibly exit it (e.g., `ctrl + C`). Thus, if automated checking over long periods is required, it is advisable to invoke `repchkdl` with an large iteration limit (e.g., `scanlim = 2222`) with a suitable polling interval (e.g., `wait = 300`). For example, like so:
````R
#Scan evern 300 seconds, 2222 times in total
test <- repchkdl(inpurl = "ftp://speedtest.tele2.net/upload/", 
                 regex = ".txt", autoscan = TRUE, autodl = TRUE, 
                 scanlim = 2222, wait = 300)
````
*  Granular control over which files should be downloaded is not available at this time (i.e., if a more than one new file is available at the URL at the time of checking, all available files files will be downloaded together).
* `ftp://`, `https://`, and `http://` are valid URL prefixes.
* At present files are downloaded using `Curl` (via `RCurl`).
* As of v.0.2.0, `repchkdl` cannot distinguish between directories/folders and files (its use of `RCurl::getBinaryURL()` precludes this). If directories are present alongside files, repchkdl will attempt to download them (and fail), and this will result in that particular download's entry being recorded as a `FAILURE` in the final output `data.frame`.


### To do's:
* Implement proper handling for IP addresses.
* Test on more FTP sites.


### Feedback and criticism welcome!
