# Basics
# SELECT <- what we want; FROM <- the database and DISTINCT <- different 
SELECT title FROM films

SELECT DISTINCT country
FROM films

# COUNT(*) in order to know how many rows are there
SELECT COUNT(*)
FROM people

SELECT COUNT(birthdate)
FROM people;

SELECT COUNT(DISTINCT country)
FROM films;

# SELECT title
# FROM films
# WHERE title = 'Metropolis';  Notice that the WHERE clause always comes after the FROM statement!

# Get all details for all films released in 2016.
SELECT *
FROM films
WHERE release_year = 2016;

# Get the number of films released before 2000.
SELECT COUNT(*)
FROM films
WHERE release_year < 2000;

# Get the title and release year of films released after 2000.
SELECT title, release_year
FROM films
WHERE release_year > 2000;

# Get all details for all French language films.
SELECT *
FROM films
WHERE language = 'French';

# Get the name and birth date of the person born on November 11th, 1974. Remember to use ISO date format ('1974-11-11')!
SELECT name, birthdate
FROM people
WHERE birthdate = '1974-11-11';

# Get the number of Hindi language films.
SELECT COUNT(*)
FROM films
WHERE language = 'Hindi';

# Get all details for all films with an R certification.
SELECT *
FROM films
WHERE certification = 'R';

# Get the title and release year for all Spanish language films released before 2000.
SELECT title, release_year
FROM films
WHERE language = 'Spanish'
AND release_year < 2000;

# Get all details for Spanish language films released after 2000.
SELECT *
FROM films
WHERE language = 'Spanish'
AND release_year > 2000;

# Get all details for Spanish language films released after 2000, but before 2010.
SELECT *
FROM films
WHERE language = 'Spanish'
AND release_year > 2000
AND release_year < 2010;

# Get the title and release year for films released in the 90s.
SELECT title, release_year
FROM films
WHERE release_year >= 1990
AND release_year < 2000;

# Now, build on your query to filter the records to only include French or Spanish language films.
SELECT title, release_year
FROM films
WHERE (language = 'French' OR language = 'Spanish')
AND release_year >= 1990
AND release_year < 2000;

# Finally, restrict the query to only return films that took in more than $2M gross.
SELECT title, release_year
FROM films
WHERE (language = 'French' OR language = 'Spanish')
AND release_year >= 1990
AND release_year < 2000
AND gross > 2000000;

# WHERE; BETWEEN
# Get the title and release year of all films released between 1990 and 2000 (inclusive).
SELECT title, release_year
FROM films
WHERE release_year
BETWEEN 1990 AND 2000;

# Now, build on your previous query to select only films that have budgets over $100 million.
SELECT title, release_year
FROM films
WHERE release_year
BETWEEN 1990 AND 2000
AND budget > 100000000;

# Now restrict the query to only return Spanish language films.
SELECT title, release_year
FROM films
WHERE release_year
BETWEEN 1990 AND 2000
AND budget > 100000000
AND language = 'Spanish';

# Finally, modify to your previous query to include all Spanish language or French language films with the same criteria as before. Don't forget your parentheses!
SELECT title, release_year
FROM films
WHERE release_year
BETWEEN 1990 AND 2000
AND budget > 100000000
AND (language = 'Spanish' OR language = 'French');

# Get the title and release year of all films released in 1990 or 2000 that were longer than two hours. Remember, duration is in minutes!
SELECT title, release_year
FROM films
WHERE release_year IN (1990, 2000)
AND duration > 120;

# Get the title and language of all films which were in English, Spanish, or French.
SELECT title, language
FROM films
WHERE language IN ('English', 'Spanish', 'French');

# Get the title and certification of all films with an NC-17 or R certification.
SELECT title, certification
FROM films
WHERE certification IN ('NC-17', 'R');

# Using IS NULL and IS NOT NULL for missing values
# Get the names of people who are still alive, i.e. whose death date is missing.
SELECT name
FROM people
WHERE deathdate IS NULL;

# Get the title of every film which doesn't have a budget associated with it.
SELECT title
FROM films
WHERE budget IS NULL;

# Get the number of films which don't have a language associated with them.
SELECT COUNT(*)
FROM films
WHERE language IS NULL;

# Using LIKE and NOT LIKE in order to search for a pattern in a column
# The % wildcard will match zero, one, or many characters in text. 
# The _ wildcard will match a single character. 
# Get the names of all people whose names begin with 'B'. The pattern you need is 'B%'.
SELECT name
FROM people
WHERE name LIKE 'B%';

# Get the names of people whose names have 'r' as the second letter. The pattern you need is '_r%'.
SELECT name
FROM people
WHERE name LIKE '_r%';

# Get the names of people whose names don't start with A. The pattern you need is 'A%'.
SELECT name
FROM people
WHERE name NOT LIKE 'A%';

# Aggregate Functions
# Use the SUM function to get the total duration of all films.
SELECT SUM(duration)
FROM films;

# Get the average duration of all films.
SELECT AVG(duration)
FROM films;

# Get the duration of the shortest film.
SELECT MIN(duration)
FROM films;

# Get the duration of the longest film.
SELECT MAX(duration)
FROM films;

# Use the SUM function to get the total amount grossed by all films.
SELECT SUM(gross)
FROM films;

# Get the average amount grossed by all films.
SELECT AVG(gross)
FROM films;

# Get the amount grossed by the worst performing film.
SELECT MIN(gross)
FROM films;

# Get the amount grossed by the best performing film.
SELECT MAX(gross)
FROM films;

# Combining aggregate functions with WHERE
# Use the SUM function to get the total amount grossed by all films made in the year 2000 or later.
SELECT SUM(gross)
FROM films
WHERE release_year >= 2000;

# Get the average amount grossed by all films whose titles start with the letter 'A'.Get the average amount grossed by all films whose titles start with the letter 'A'.
SELECT AVG(gross)
FROM films
WHERE title LIKE 'A%';

# Get the amount grossed by the worst performing film in 1994.
SELECT MIN(gross)
FROM films
WHERE release_year = 1994;

# Get the amount grossed by the best performing film between 2000 and 2012, inclusive.
SELECT MAX(gross)
FROM films
WHERE release_year BETWEEN 2000 AND 2012;

# It's AS simple AS aliasing
# Aliasing simply means you assign a temporary name to something. To alias, you use the AS keyword
# Get the title and net profit (the amount a film grossed, minus its budget) for all films. Alias the net profit as net_profit.
SELECT title,
       gross - budget AS net_profit
FROM films;

# Get the title and duration in hours for all films. The duration is in minutes, so you'll need to divide by 60.0 to get the duration in hours. Alias the duration in hours as duration_hours.
SELECT title,
       duration/60.0 AS duration_hours
FROM films;

# Get the average duration in hours for all films, aliased as avg_duration_hours.
SELECT AVG(duration)/60.0 AS avg_duration_hours
FROM films;

# Get the percentage of people who are no longer alive. Alias the result as percentage_dead. Remember to use 100.0 and not 100!
SELECT COUNT(deathdate) * 100.0 / COUNT(*) AS percentage_dead
FROM people;

# Get the number of years between the newest film and oldest film. Alias the result as difference.
SELECT MAX(release_year) - MIN(release_year) AS difference
FROM films;

# Get the number of decades the films table covers. Alias the result as number_of_decades. The top half of your fraction should be enclosed in parentheses.
SELECT (MAX(release_year) - MIN(release_year)) / 10.0 
AS number_of_decades
FROM films;

# ORDER BY
# the ORDER BY keyword is used to sort results in ascending or descending order according to the values of one or more columns.
# Get the names of people from the people table, sorted alphabetically.
SELECT name
FROM people
ORDER BY name;

# Get the names of people, sorted by birth date.
SELECT name
FROM people
ORDER BY birthdate;

# Get the birth date and name for every person, in order of when they were born.
SELECT birthdate, name
FROM people
ORDER BY birthdate;

# Get the title of films released in 2000 or 2012, in the order they were released.
SELECT title
FROM films
WHERE release_year IN (2000, 2012)
ORDER BY release_year;

# Get all details for all films except those released in 2015 and order them by duration.
SELECT *
FROM films
WHERE release_year <> 2015
ORDER BY duration;

# Get the title and gross earnings for movies which begin with the letter 'M' and order the results alphabetically.
SELECT title, gross
FROM films
WHERE title LIKE 'M%'
ORDER BY title;

# To order results in descending order, you can put the keyword DESC after your ORDER BY
# Get the IMDB score and film ID for every film from the reviews table, sorted from highest to lowest score.
SELECT imdb_score, film_id
FROM reviews
ORDER BY imdb_score DESC;

# Get the title for every film, in reverse order.
SELECT title
FROM films
ORDER BY title DESC;  

# Get the title and duration for every film, in order of longest duration to shortest.
SELECT title, duration
FROM films
ORDER BY duration DESC;

# ORDER BY can also be used to sort on multiple columns.
# Get the birth date and name of people in the people table, in order of when they were born and alphabetically by name.
SELECT birthdate, name
FROM people
ORDER BY birthdate, name;

# Get the release year, duration, and title of films ordered by their release year and duration.
SELECT release_year, duration, title
FROM films
ORDER BY release_year, duration;

# Get certifications, release years, and titles of films ordered by certification (alphabetically) and release year.
SELECT certification, release_year, title
FROM films
ORDER BY certification, release_year;

# Get the names and birthdates of people ordered by name and birth date.
SELECT name, birthdate
FROM people
ORDER BY name, birthdate;

# GROUP BY
# In SQL, GROUP BY allows you to group a result by one or more columns, like so:
# Note that GROUP BY always goes after the FROM clause!
# Get the release year and count of films released in each year.
SELECT release_year, COUNT(*)
FROM films
GROUP BY release_year;

# Get the release year and average duration of all films, grouped by release year.
SELECT release_year, AVG(duration)
FROM films
GROUP BY release_year;

# Get the release year and largest budget for all films, grouped by release year.
SELECT release_year, MAX(budget)
FROM films
GROUP BY release_year;

# Get the IMDB score and count of film reviews grouped by IMDB score in the reviews table.
SELECT imdb_score, COUNT(*)
FROM reviews
GROUP BY imdb_score;

# Get the release year and lowest gross earnings per release year.
SELECT release_year, MIN(gross)
FROM films
GROUP BY release_year;

# Get the language and total gross amount films in each language made.
SELECT language, SUM(gross)
FROM films
GROUP BY language;

# Get the country and total budget spent making movies in each country.
SELECT country, SUM(budget)
FROM films
GROUP BY country;

# Get the release year, country, and highest budget spent making a film for each year, for each country. Sort your results by release year and country.
SELECT release_year, country, MIN(gross)
FROM films
GROUP BY country, release_year
ORDER BY country, release_year;

# In SQL, aggregate functions can't be used in WHERE clauses.
# That's where the HAVING clause comes in.
SELECT release_year
FROM films
GROUP BY release_year
HAVING COUNT(title) > 200;

# Get the release year, budget and gross earnings for each film in the films table.
SELECT release_year, budget, gross
FROM films

# Modify your query so that only records with a release_year after 1990 are included.
SELECT release_year, budget, gross
FROM films
WHERE release_year > 1990;

# Remove the budget and gross columns, and group your results by release year.
SELECT release_year
FROM films
WHERE release_year > 1990
GROUP BY release_year;

# Modify your query to include the average budget and average gross earnings for the results you have so far. Alias the average budget as avg_budget; alias the average gross earnings as avg_gross.
SELECT release_year,
		AVG(budget) AS avg_budget, 
		AVG(gross) AS avg_gross
FROM films
WHERE release_year > 1990
GROUP BY release_year;

# Modify your query so that only years with an average budget of greater than $60 million are included.
SELECT release_year, AVG(budget) AS avg_budget, AVG(gross) AS avg_gross
FROM films
GROUP BY release_year
HAVING AVG(budget) > 60000000;

# Finally, modify your query to order the results from highest average gross earnings to lowest.
SELECT release_year, AVG(budget) AS avg_budget, AVG(gross) AS avg_gross
FROM films
GROUP BY release_year
HAVING AVG(budget) > 60000000
ORDER BY avg_gross DESC;

# you can use the LIMIT keyword to limit the number of rows returned
# Get the country, average budget, and average gross take of countries that have made more than 10 films. Order the result by country name, and limit the number of results displayed to 5. You should alias the averages as avg_budget and avg_gross respectively.
-- select country, average budget, average gross
SELECT country, 
        AVG(budget) AS avg_budget,
        AVG(gross) AS avg_gross
-- from the films table
FROM films
-- group by country 
GROUP BY country
-- where the country has more than 10 titles
HAVING COUNT(title) > 10 
-- order by country
ORDER BY country
-- limit to only show 5 results
LIMIT 5

# You can also combine two tables
SELECT title, imdb_score
FROM films
JOIN reviews
ON films.id = reviews.film_id
WHERE title = 'To Kill a Mockingbird';