
# Params
data_path = './data/base/Land-Prices_DLOS_2019-January.xlsx'

library(dplyr)
library(readxl)
library(scales)
library(tigris)
library(tmap)
library(tmaptools)

options(tigris_class = 'sf')
options(tigris_use_cache = T)

title = 'Land Price per Acre*\n% Change, 2012-2017'

caption = '* Under single family homes\nData: Federal Housing Finance Authority,\n\t  William Larson @larsonwd,\n\t  Jessica Shui, Morris Davis,\n\t  Stephen Oliner\nMap: @TheRealEveret'

nj_land_sum =
  read_excel(path = data_path,
             sheet = 'Panel ZIP Codes') %>%
  select(-`YoY % Change`) %>%
  group_by(`ZIP Code`) %>%
  summarize(
    pct_chg_2012_2017 =
      (`Land Price`[Year == 2017] / `Land Price`[Year == 2012]) - 1
  )

# Hmm, defining the `state` argument in `tigris::zctas` seem to do nothing.
# A full map of all US ZCTAS is downloaded, regardless(!!).
# Submitted an issue on Github: https://github.com/walkerke/tigris/issues/71
# Until resolved, this is commented out. Rest of script won't run.
# nj_zips_land =
  # tigris::zctas(cb = T, year = 2000, state = 'NJ') %>%
  # left_join(nj_land_sum, by = c('NAME' = 'ZIP Code'))

tmap_mode('plot')

nj_zips_land_map = nj_zips_land %>%
  tm_shape() +
  tm_borders(col = 'black') +
  tm_fill(col = 'pct_chg_2012_2017',
          palette = tmaptools::get_brewer_pal('RdYlGn', n = 5,
                                              contrast = c(0, 1)),
          colorNA = 'grey',
          title = title,
          legend.reverse = T,
          style = 'cont') +
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
