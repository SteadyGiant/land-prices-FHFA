cat('\014')

# Params
data_path = './data/base/Land-Prices_DLOS_2019-January.xlsx'

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(scales)
  library(tigris)
  library(tmap)
  library(tmaptools)
})

suppressWarnings({
  options(tigris_class = 'sf')
  options(tigris_use_cache = T)
})

title = 'Land Price per Acre*\n% Change, 2012-2017'

caption = '* Under single family homes\nData: Federal Housing Finance Authority,\n\t  William Larson @larsonwd,\n\t  Jessica Shui, Morris Davis,\n\t  Stephen Oliner\nMap: @TheRealEveret'

land_prices = read_excel(path = data_path,
                         sheet = 'Panel County')

nj_land = land_prices %>%
  select(-`YoY % Change`) %>%
  filter(State == 'New Jersey'
         & Year %in% c(2012, 2017)) %>%
  mutate(County = gsub(' County', '', County))

nj_land_sum = nj_land %>%
  group_by(County) %>%
  summarize(
    pct_chg_2012_2017 =
      (`Land Price`[Year == 2017] / `Land Price`[Year == 2012]) - 1
  )

nj_counties_land = tigris::counties(cb = T, year = 2015) %>%
  filter(STATEFP == 34) %>%
  left_join(nj_land_sum, by = c('NAME' = 'County'))

tmap_mode('plot')

NJ_counties_land_map = nj_counties_land %>%
  tm_shape() +
  tm_fill(col = 'pct_chg_2012_2017',
          palette = tmaptools::get_brewer_pal('RdYlGn', n = 5,
                                              contrast = c(0, 1)),
          title = title,
          legend.reverse = T,
          style = 'cont') +
  tm_borders(col = 'black') +
  tm_credits(text = caption,
             position = c('right', 'bottom')) +
  tm_layout(title = '',
            legend.format = list(fun = scales::percent_format(accuracy = 1)),
            legend.position = c('left', 'center'),
            legend.title.size = 1,
            legend.text.size = 0.7,
            frame = F,
            inner.margins = 0.1,
            asp = 1)

tmap::tmap_save(tm = NJ_counties_land_map,
                filename = './graphics/NJ_counties_landprice.png')

readr::write_csv(x = nj_land_sum,
                 path = './data/processed/NJ_counties_landprice.csv')
