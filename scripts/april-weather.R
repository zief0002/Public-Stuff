library(dplyr)
library(forcats)
library(ggplot2)
library(ggridges)
library(readr)
library(viridis)


april = read_csv("~/Desktop/april-weather.csv")

april2 = april %>%
  filter(date <= 11)


## Ridge plot 
## Inspiration: https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html
## http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/

p = ggplot(april2, aes(x = temp_avg, y = fct_rev(factor(year)), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(
    name = expression(paste('Temp. (',~degree,'F)',sep='')), 
    option = "C"
  ) +
  labs(
    title = 'April Temperatures in Minneapolis',
    subtitle = 'Mean temperatures for the first 11 days in April',
    x = expression(paste('Average Daily Temperature (',~degree,'F)',sep=''))
  ) +
  theme_ridges(
    font_size = 13, 
    grid = TRUE
  ) + 
  theme(
    axis.title.y = element_blank()
  )


ggsave(p, filename = "~/Desktop/this-april-sucks.png", width = 10, height = 6)

#### Counts

april2 %>%
  group_by(year) %>%
  summarize(
    days_with_snow = length(grep(pattern = "Snow", x = events)),
    days_over_40 = sum(temp_high > 40),
    days_over_50 = sum(temp_high > 50),
    days_over_60 = sum(temp_high > 60)
  )





### Heatmap
### Inspiration: https://kieranhealy.org/blog/archives/2018/04/10/visualizing-the-baby-boom/


label = substitute(paste('Daily average temperature (', ~degree, 'F) for the first 11 Days in April', sep = ''))

ggplot(april2, aes(x = factor(date), y = fct_rev(factor(year)), fill = temp_high)) + 
  geom_tile(color = "white") +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(
    name = "", 
    option = "C"
  ) +
  labs(
    title = 'Daily Average Temperature in Minneapolis',
    subtitle = label,
    x = 'Date',
    y = ''
  ) +
  theme(
    legend.position = "top", 
    legend.justification = c(0, 0),
    legend.direction = "horizontal"
  )







#### LOW TEMP

ggplot(april2, aes(x = temp_low, y = fct_rev(factor(year)), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(
    name = expression(paste('Temp. (',~degree,'F)',sep='')), 
    option = "C"
  ) +
  labs(
    title = 'April Temperatures in Minneapolis',
    subtitle = 'Daily Low temperatures for the first 11 Days in April',
    x = expression(paste('Daily Low Temperature (',~degree,'F)',sep=''))
  ) +
  theme_ridges(
    font_size = 13, 
    grid = TRUE
  ) + 
  theme(
    axis.title.y = element_blank()
  )



#### HIGH TEMP

ggplot(april2, aes(x = temp_high, y = fct_rev(factor(year)), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(
    name = expression(paste('Temp. (',~degree,'F)',sep='')), 
    option = "C"
  ) +
  labs(
    title = 'April Temperatures in Minneapolis',
    subtitle = 'Daily High temperatures for the first 11 Days in April',
    xlab = expression(paste('Daily High Temperature (',~degree,'F)',sep=''))
  ) +
  theme_ridges(
    font_size = 13, 
    grid = TRUE
  ) + 
  theme(
    axis.title.y = element_blank()
  )

