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
    "_merlin_key"="SFMyNTY.g3QAAAADbQAAAAtfY3NyZl90b2tlbm0AAAAYYWxGbUU2WnRBZ2tibEstNG4xRFZWSVFBbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAmRleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTJNVEk1T0RVeE9UTXNJbWxoZENJNk1UWXhNams0TVRVNU15d2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pT1dJd1pESTFNall0TURjME15MDBPVGMyTFRrMlpHTXRNbVEyWm1JeU0yRTNZalJrSWl3aWJtSm1Jam94TmpFeU9UZ3hOVGt5TENKd1pXMGlPbnNpWTI5c2JHVm5aU0k2TVN3aVpXeHBkR1VpT2pGOUxDSnpkV0lpT2lKN1hDSmxiV0ZwYkZ3aU9sd2labXh2TG1KcFpXeHRaV2xsY2tCbmJXRnBiQzVqYjIxY0lpeGNJbVpsWVhSMWNtVnpYQ0k2VzEwc1hDSm1hWEp6ZEY5dVlXMWxYQ0k2WENKR2JHOXlhV0Z1WENJc1hDSnNZWE4wWDI1aGJXVmNJanBjSWtKcFpXeHRaV2xsY2x3aUxGd2lkV2xrWENJNlhDSmlNbVk0TjJFMllpMHhZbVEzTFRRelpqWXRPVGM0TlMwMk1qSmhPR0ptT1RSaE5UVmNJaXhjSW5abGNuUnBZMkZzWENJNlhDSkRiMjV6ZFcxbGNsd2lmU0lzSW5SNWNDSTZJbUZqWTJWemN5SjkuRVp3dTh3bjVUazVhSmdiaEg2aWo1Y1lSLXNYM0tYUlluZjUySUI3SFhFaGlYYXJob1QyeGY2T0ZfMmV4cExXUmtrS0t2dU9YRlk5a0U4RUpSOEZKTWdtAAAACXJldHVybl90b20AAAAOL2ZhbnRhc3kvc3RhdHM.G9ZY5dQyl086t_TkxhgFK8oIwfb2tk_gPgJHriTaWq8",
    "c_groot_access_token"="Q4AkFTQuRf8KmbV12NuDFgoB4rolXga7aRho3JbpEMZeqMe6B8dddiBHzIMi4sTi",
    # "c_groot_access_ts"="2020-12-07T15:33:44Z",
    "c_groot_refresh_token"="3P1Yoiw6Mpt0Upq7h8B7bD5sGBwGUb6eHcEgtBqfp54bxlZ-921qmLVQfTZDApiY",
    "AWSALB"="94+lwqD8w6a9vcTCt1z37hJPXEJz1Xqs5b1DId5ujsIV8Yti4oEyIopB4C3xieP3AYWM9L9O9BBX7Jtg8h6FZBZbIe+vOumgqTBtukoJ5Y2eoPTIskwikQXREvdz",
    "AWSALBCORS"="94+lwqD8w6a9vcTCt1z37hJPXEJz1Xqs5b1DId5ujsIV8Yti4oEyIopB4C3xieP3AYWM9L9O9BBX7Jtg8h6FZBZbIe+vOumgqTBtukoJ5Y2eoPTIskwikQXREvdz"
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

write.csv2(df,"H:/GitHub/MBSL/preparation/fpts18-20.csv")
