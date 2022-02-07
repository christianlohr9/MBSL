library(tidyverse)

Sys.setenv(http_proxy = "172.30.15.242:8080")
Sys.setenv(https_proxy = "172.30.7.242:8080")

position <- c("passing","receiving")
season <- 2019:2021

get_fpts <- function(position,season) {
raw <- httr::GET(paste0(
  "https://www.pff.com/api/fantasy/stats/",
  position,
  "?&season=",
  season,
  "&weeks=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16&scoring=139038"),
  httr::set_cookies(
    "_ga_8Y6RN784SW"="GS1.1.1644234063.2.0.1644234063.60",
    "_merlin_key"="SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYUzBCcnVzeGc2TUV2SlY4eUtKUk52c0dHbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAlNleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTJORFF5TXpjMk5qRXNJbWxoZENJNk1UWTBOREl6TkRBMk1Td2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pTlRjd016SmtZV1F0TlRVMk9TMDBNRGRtTFdJeVpEWXRNV0UzT1dZME5tUTNNRGMwSWl3aWJtSm1Jam94TmpRME1qTTBNRFl3TENKd1pXMGlPbnNpWTI5c2JHVm5aU0k2TVN3aVpXeHBkR1VpT2pGOUxDSnpkV0lpT2lKN1hDSmxiV0ZwYkZ3aU9sd2lZMnh2YUhJNVFHZHRlQzVrWlZ3aUxGd2labVZoZEhWeVpYTmNJanBiWFN4Y0ltWnBjbk4wWDI1aGJXVmNJanBjSWtOb2NtbHpkR2xoYmx3aUxGd2liR0Z6ZEY5dVlXMWxYQ0k2WENKTWIyaHlYQ0lzWENKMWFXUmNJanBjSW1SaFl6QXdNRGhpTFRVMk1tSXROR1l6TUMxaE1EUTJMV0k1TldRNU1XRmpNekV4Tmx3aUxGd2lkbVZ5ZEdsallXeGNJanBjSWtOdmJuTjFiV1Z5WENKOUlpd2lkSGx3SWpvaVlXTmpaWE56SW4wLlZWUDFoYTFCck1lVHNMYk12V2xCVWNZUVF1dDBWNTVzRlRweGd0amhydWhZbTFyVnlRSFpaUUU2MHJPajdtYUJaRjhNNWlTRDUtZUJWOXdra1ZQZ0R3bQAAAAlyZXR1cm5fdG9tAAAADi9mYW50YXN5L3N0YXRz.0FfOJB3ESVjdDJWb2Dlp7jaiVIAjPdz5LpoXcioN0wI",
    "c_groot_access_token"="33RYFF8lMagKouJ2Yb7Dud6UlZdGAfqUmc3-rwrujbgctJgaSm4lMmlb3oCEp9LB",
    # "c_groot_access_ts"="2020-12-07T15:33:44Z",
    "c_groot_refresh_token"="nSed1Rw52oLPMWiV6T8Fyz3r16kAeeUUlwJDkEll5wHq0H1i6VTbKWOREtzvElIJ",
    "AWSALB"="xbO+LD+mj5yXEbjMJEX9366LfFSQRj9JsETsxB8QLPjVgDYPm50siAnPGBkyCYslTAbb9YQrYP2y5p50FWvCchjypkgDKGTn3j7RkJgjIqexr34eS31L1LqPApsd",
    "AWSALBCORS"="xbO+LD+mj5yXEbjMJEX9366LfFSQRj9JsETsxB8QLPjVgDYPm50siAnPGBkyCYslTAbb9YQrYP2y5p50FWvCchjypkgDKGTn3j7RkJgjIqexr34eS31L1LqPApsd"
  )
)
content <- httr::content(raw) %>%
  transpose() %>%
  as_tibble() %>% 
  select(player_id,fantasy_pts) %>% 
  mutate(year=season)

return(content)

}

all_fpts <- purrr::pmap_dfr(purrr::transpose(
  purrr::cross2(position,season)), get_fpts)

df = data.frame(lapply(all_fpts, as.character), stringsAsFactors=FALSE)

sleeper_id <- nflfastR::fast_scraper_roster(2021) %>% 
  select(pff_id,sleeper_id, position) %>% 
  mutate_if(is.numeric, as.character)

df_id <- df %>% 
  left_join(sleeper_id, by = c("player_id"="pff_id"))

write.csv2(df_id,"Z:/GitHub/MBSL/preparation/fpts19-21.csv")
