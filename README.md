# repchkdl
`repchkdl` is an R package that consists of a single, eponymous function that--given a URL hosting a collection of files--will repeatedly check if new files have been added to the URL (since the first time it checks the URL). If new files are available, repchkdl indicates the files and offers to download them. An optional regular expression pattern can be specified to constrain what files repchkdl should consider as valid new additions (based on the file name(s)). repchkdl saves the downloaded files to the default working directory unless an alternative path is specified. When invoked for the first time, `repchkdl` will offer to download all available files (if a regular expression is supplied, then all files matching the expression). Options to control whether files are actually downloaded or not, the frequency at which the remote location should be polled, and manual control over scanning and downloading (among other things) are available (please refer to the package documentation for details). 

### Example usage:
````R
test <- repchkdl(inpurl = "ftp://speedtest.tele2.net/upload/", 
                 inpregex = ".txt", autoscan = "n", autodl = "n")
````

### Notes:
* If `autoscan` is set to `"y"`, `repchkdl` will loop indefinitely! The only way to kill the script then is to forcibly exit it (e.g., `ctrl + C`). Thus, if automated checking over long periods is required, it is advisable to invoke `repchkdl` with an iteration limit (via the `scanlim` parameter) with a desired polling interval (`inpwait`). For example, like so:

````R
test <- repchkdl(inpurl = "ftp://speedtest.tele2.net/upload/", 
                 inpregex = ".txt", autoscan = "y", autodl = "n", 
                 scanlim = 2)
````


### Feedback and criticism welcome!
