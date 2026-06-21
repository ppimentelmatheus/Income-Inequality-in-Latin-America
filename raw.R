dir.create("RAIS")


dir.create("RAIS/raw")
dir.create("RAIS/intermediate")
dir.create("RAIS/final")
dir.create("RAIS/scripts")


# Acessar FTP dentro do R -------------------------------------------------

library(curl)
library(RCurl)
library(archive)

# Verificar se a estrutura de acesso esta funcionando

ftp_url = "ftp://ftp.mtps.gov.br/pdet/microdados/"
curl_fetch_memory(ftp_url)
readLines("ftp://ftp.mtps.gov.br/pdet/microdados/")
con <- url(
  "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/",
  "r"
)
readLines(con)
getURL(
  "ftp://ftp.mtps.gov.br/pdet/microdados/",
  ftp.use.epsv = FALSE,
  dirlistonly = TRUE
)

# Para 1994

con94 <- url(
  "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/1994/",
  "r"
)

readLines(con94)

close(con94)

# Vamos baixar inicialmente o acre

download.file(
  url = "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/1994/AC1994.7z",
  destfile = "RAIS/raw/AC1994.7z",
  mode = "wb"
)
# Verificar
file.info("RAIS/raw/AC1994.7z")

archive_extract(
  "RAIS/raw/AC1994.7z",
  dir = "RAIS/raw/AC1994"
)


list.files(
  "RAIS/raw/AC1994",
  recursive = TRUE,
  full.names = TRUE
)

readLines(
  "RAIS/raw/AC1994/AC1994.txt",
  n = 5)

nchar(readLines(
  "RAIS/raw/AC1994/AC1994.txt",
  n = 10
))


library(readr)


