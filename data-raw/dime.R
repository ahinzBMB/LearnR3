library(here)
library(fs)

dime_link <- "https://github.com/rostools/r-cubed-intermediate/raw/refs/heads/main/data/dime.zip"
download.file(dime_link, destfile = here("data-raw/dime.zip"))


# Remove leftover folder so unzipping is always clean
dir_delete(here("data-raw/dime"))

unzip(
  here("data-raw/dime.zip"),
  exdir = here("data-raw/dime/")
)
