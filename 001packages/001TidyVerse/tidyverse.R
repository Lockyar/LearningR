library(gapminder)
library(dplyr)

gapminder

#filtering data.
gapminder %>% filter(year == 1957)
gapminder %>% filter(year == 1957, country == "China")

#arranging the data in descending or ascending by filtering the year.
gapminder %>% filter(year == 1957) %>% arrange(pop)
gapminder %>% filter(year == 1957) %>% arrange(desc(pop))

#mutating the data -> this allows to add a new column or modify an existing one.
gapminder %>% mutate(lifeExp = 12 * lifeExp)
gapminder %>% mutate(lifeExpMonths = 12 * lifeExp)

#Using all three
gapminder %>% filter(year == 2007) %>% mutate(lifeExpMonths = 12 * lifeExp) %>% arrange(desc(lifeExpMonths))

#Using ggplot2
library(ggplot2)

#Saving and filtering a data
gapminder_1952 <- gapminder %>% filter(year == 1952)

#Using ggplot to create a point graph -> geom_point()
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) + geom_point()

#Using log scale in order to make better graphs
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) + geom_point() + scale_x_log10()
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) + geom_point() + scale_y_log10()

#Adding color and size to the graph
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent)) + geom_point() + scale_x_log10()
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) + geom_point() + scale_x_log10()

#Faceting or subgraphs with facet_wrap(~ variable(x))
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) + geom_point() + scale_x_log10() + facet_wrap(~ continent)
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + geom_point() + scale_x_log10() + facet_wrap(~ year)

#Summarizing a data set
gapminder %>% summarize(medianLifeExp = median(lifeExp))

#Filtering and summarizing
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp = median(lifeExp))
gapminder %>% filter(year == 1957) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

#The group_by verb
gapminder %>% group_by(year) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))
gapminder %>% filter(year == 1957) %>% group_by(continent) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

gapminder %>% group_by(continent, year) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

#Using ggplot and group_by to visualize data
by_year <- gapminder %>% group_by(year) %>% summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))
ggplot(by_year, aes(x = year, y = medianLifeExp)) + geom_point() + expand_limits(y = 0)

by_year_continent <- gapminder %>% group_by(continent, year) %>% summarize(medianGdpPercap = median(gdpPercap))
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) + geom_point() + expand_limits(y = 0)

by_year_2007 <- gapminder %>% filter(year == 2007) %>% group_by(continent) %>% summarize(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))
ggplot(by_year_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent)) + geom_point() + expand_limits(y = 0)
