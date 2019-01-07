library(gapminder)
library(dyplr)

print(gapminder)

gapminder %>% filter(year == 1957)