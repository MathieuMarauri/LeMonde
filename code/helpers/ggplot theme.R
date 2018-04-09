palette_custom <- ggthemr::define_palette(
  swatch = c(
    'royalblue4',
    'dodgerblue4', 
    'lightsteelblue4', 
    'maroon', 
    'peru',
    'burlywood4', 
    'navajowhite2', 
    'lightpink4'
  ),
  gradient = c('dodgerblue4', 'maroon'),
  background = "#ffffff",
  text = c('grey30', 'grey30'), 
  line = c('grey30', 'grey30'),
  gridline = 'grey80'
)

theme_custom <- ggthemr::ggthemr(
  palette = palette_custom,
  type = 'outer',
  spacing = 1, 
  layout = 'clear2',
  text_size = 15,
  set_theme = TRUE
)