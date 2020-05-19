# repchkdl
Given a URL hosting a collection of files, repchkdl will repeatedly check if new files have been added to the URL (since the first time it checks the URL). If new files are available, repchkdl indicates the files and offers to download them. An optional regular expression pattern can be specified to constrain what files repchkdl should consider as valid new additions (based on the file name(s)). repchkdl saves the downloaded files to the default working directory unless an alternative path is specified.

### Usage:
test <- repchkdl(inpurl = "ftp://speedtest.tele2.net/upload/", inpregex = ".txt", autoscan = "n", autodl = "n")
