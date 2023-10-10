
# Library -----------------------------------------------------------------

library(tidyverse)



# Data --------------------------------------------------------------------

saga_chap  <- read_rds('sagas_chapter.rds')
akuma_no_mi <- read_rds('akuma_no_mi.rds') %>% 
  distinct()

# Cleaning ----------------------------------------------------------------

akuma_no_mi %>%  
  mutate(Usage_manga = Usage_manga %>%  
           str_replace_all('Chapter ', '') %>% 
           as.numeric(),
         Usage_anime = Usage_anime %>%  
           str_replace_all('Episode ', '') %>% 
           as.numeric()
           ) %>%  view()

akuma_no_mi %>% 
  filter(!(Usage_anime %>%  str_detect("Episode"))) %>%
  select(Akuma_name, Usage_manga, Usage_anime, Url) %>% 
  view()

akuma_no_mi %>% mutate(
  usage_type_manga = case_when(
    Usage_manga %>%  str_detect('Chapter') ~ 'manga', 
    Usage_manga %>%  str_detect('Episode') ~ 'anime',
    Usage_manga %>%  str_detect('Movie') ~ 'movie',
    Usage_manga %>%  str_detect('Film') ~ 'movie',
    Usage_manga %>%  str_detect('Stampede') ~ 'movie',
    Usage_manga %>%  str_detect('Clockwork Island Adventure') ~ 'movie',
    Usage_manga %>%  str_detect('Premier Show') ~ 'premier',
    Usage_manga %>%  str_detect('3D2Y') ~ 'special',
    Usage_manga %>%  str_detect('Heart of Gold') ~ 'special',
    Usage_manga %>%  str_detect("Fischer's x One Piece") ~ 'spinoff', 
    Usage_manga %>%  str_detect("Lucy & Lucy Coliseum the Battle!") ~ 'videogame', 
    Usage_manga %>%  str_detect("One Piece Dramatic Stage THE METAL") ~ 'videogame', 
    Usage_manga %>%  str_detect("One Piece Odyssey") ~ 'videogame', 
    Usage_manga %>%  str_detect("Ocean's Dream") ~ 'videogame', 
    Usage_manga %>%  str_detect("Unlimited World Red") ~ 'videogame',
    Usage_manga %>%  str_detect("One Piece Art NUE") ~ 'artwork',
    Usage_manga %>%  str_detect("novel") ~ 'novel',
    
    
  ) 
)

Episodes   <- c(1, NA, 635, 827, 832,
                NA, NA, NA, NA, NA,
                NA, NA, 54, 56, NA,
                NA, NA, 326,NA,)
Chapter <- c(1, 428, 714, 864, 868,
             1063, 1063, 1063, 1066, 1080,
             1080, 1080, NA, NA, NA,
             NA,NA, NA,NA,
             )
# Merging -----------------------------------------------------------------


