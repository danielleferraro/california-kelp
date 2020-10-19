Download kelp area shapefiles from California Dept. of Fish and Wildlife
================
Danielle Ferraro, NCEAS -
2020-10-19

Shapefiles of kelp coverage from aerial surveys of California were
accessed at the California Department of Fish and Wildlife
[website](https://wildlife.ca.gov/Conservation/Marine/Kelp/Aerial-Kelp-Surveys).
Surveys were conducted in 1989, 1999, and annually from 2002-2016.
Below, the resulting shapefiles are downloaded directly from their FTP
server and saved in this repository at `raw/BIO_*`. Note that several of
the files exceed the GitHub storage limit, so the `raw/` directory will
not be tracked on GitHub.

``` r
library(here)

# Set up URL and list of files to download
url <- "ftp://ftp.dfg.ca.gov/R7_MR/BIOLOGICAL/Kelp/"
files <- RCurl::getURL(url, dirlistonly = TRUE)
files <- unlist(strsplit(files, '\n'))

# Download each file and unzip
for (i in files) {
  download.file(paste(url, i, sep = ""), paste0(here("raw"), "/", i))
  unzip(paste0(here("raw"), "/", i), exdir = here("raw"))
}
```

A historical map of kelp coverage from 1911 was digitized by the UCSB
Collaboratory and provided by C. Scarborough. It is saved at
`raw/CalforniaKelp.gdb`.
