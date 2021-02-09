library(tiyverse)

Sys.setenv(http_proxy = "172.30.15.242:8080")
Sys.setenv(https_proxy = "172.30.7.242:8080")

position <- c("passing","receiving")
season <- 2018:2020

get_fpts <- function(position,season) {
raw <- httr::GET(paste0(
  "https://www.pff.com/api/fantasy/stats/",
  position,
  "?&season=",
  season,
  "&weeks=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16&scoring=139038"),
  httr::set_cookies(
    "__cfduid"="d254bdc173ac48cbf9b265f8bc689ccea1612280085",
    "mailmunch_second_pageview"="true",
    "_mailmunch_visitor_id"="60a87cce-f757-4045-8ab7-d50ed3b1feef",
    "_gcl_au"="1.1.1467758914.1612280088",
    "_gid"="GA1.2.1385330789.1612648710",
    "seerses"="e",
    "_fbp"="fb.1.1612280088919.1025354076",
    "seerid"="u_841648512495216600",
    "_ga_8Y6RN784SW"="GS1.1.1612648710.8.1.1612649437.56",
    "_ga"="GA1.2.1078851802.1612280089",
    "_merlin_key"="SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYYWxGbUU2WnRBZ2tibEstNG4xRFZWSVFBbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAmRleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTJNVEkyTlRJek1Ea3NJbWxoZENJNk1UWXhNalkwT0Rjd09Td2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pT0dJd1pqVXhOelV0WldKbE1DMDBaV05qTFdFNE56SXRZelF6TkdZMllXSTJNVFV4SWl3aWJtSm1Jam94TmpFeU5qUTROekE0TENKd1pXMGlPbnNpWTI5c2JHVm5aU0k2TVN3aVpXeHBkR1VpT2pGOUxDSnpkV0lpT2lKN1hDSmxiV0ZwYkZ3aU9sd2labXh2TG1KcFpXeHRaV2xsY2tCbmJXRnBiQzVqYjIxY0lpeGNJbVpsWVhSMWNtVnpYQ0k2VzEwc1hDSm1hWEp6ZEY5dVlXMWxYQ0k2WENKR2JHOXlhV0Z1WENJc1hDSnNZWE4wWDI1aGJXVmNJanBjSWtKcFpXeHRaV2xsY2x3aUxGd2lkV2xrWENJNlhDSmlNbVk0TjJFMllpMHhZbVEzTFRRelpqWXRPVGM0TlMwMk1qSmhPR0ptT1RSaE5UVmNJaXhjSW5abGNuUnBZMkZzWENJNlhDSkRiMjV6ZFcxbGNsd2lmU0lzSW5SNWNDSTZJbUZqWTJWemN5SjkuUkZaY0dCenVIbmpzdFlVUkJCT3l1ejNsd2VsOUlUbnVKV0RybjBLZldQZTg5THg1WDdNdzItbVlEWlVlME9wREZhamVhUWpFNFJNSnBPZEVlbkc5SlFtAAAACXJldHVybl90b20AAAAOL2ZhbnRhc3kvc3RhdHM.K6ZW4tEOebWrmBfZep3YMOiduZPZXiIGrdTGhpUssvU",
    "c_groot_access_token"="vL_gqHRn6aTyIjzEWbIhh5Az6yQSTY_New3v-8nITvWB-UMEfrkp5xneVpGbs3HB",
    # "c_groot_access_ts"="2020-12-07T15:33:44Z",
    "c_groot_refresh_token"="_K-3tkJD5zsmK5SlxrfyD2cJxLJEDsKJbtnwIiSOFT9mYVkpd_-lVXBz3e1Wk5jq",
    "AWSALB"="+B0ue7gfdvVPu8LBqX25xM+vTa1UUPDuqUnPjw7XwSl8UclR2wjeUwmpvQbU8yiZbX3Ht6cpVY5xk9PdlHocT5vLT+5u/LS+awqIQyV180i0u/VQVznrNxeV0Zgj",
    "AWSALBCORS"="+B0ue7gfdvVPu8LBqX25xM+vTa1UUPDuqUnPjw7XwSl8UclR2wjeUwmpvQbU8yiZbX3Ht6cpVY5xk9PdlHocT5vLT+5u/LS+awqIQyV180i0u/VQVznrNxeV0Zgj"
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

write.csv2(df,"fpts18-20.csv")
