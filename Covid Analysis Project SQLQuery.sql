--COVID-19 Analysis Project
  --1. Introduction

    --The Covid pandemic has been affecting the world since late 2019, 
    --causing unprecendented impacts on the health and economy of different countries and regions.
    --This project aims to analyze the data and provide insights related to the COVID-19 testing, 
    --vaccination, and mortality across various locations and time periods.
	--It will especially examine how much damage covid 19 has caused to the world and 
	--if the vaccinations and boosters were effective.

  --2. Data Source

    --The data used in this project comes from Our World in Data Coronavirus (COVID-19) Vaccinations
	--and Coronavirus (COVID-19) Deaths datasets. 
	--The Coronavirus (COVID-19) Vaccinations dataset contains information about the COVID-19 testing 
	--and vaccination activities, as well as some demographic and socio-economic indicators, for each location and date.
	--The Coronavirus (COVID-19) Deaths dataset contains information about the COVID-19 cases and deaths, 
	--as well as some health and hospitalization indicators, for each location and date.
  
  --3. Data Overview
	SELECT *
	FROM CovidVaccinations;

	SELECT *
	FROM CovidDeaths;
	--The datesets contain data updated until when I downloaded them. The numbers in the "total" columns
	--are aggregated by each date.

  --4. Data Exploration

	--Check the data quality and completeness:
	--In this case, I want to count the number of missing values for all the columns.
	DECLARE @sql_v nvarchar(max) = N'SELECT * FROM (SELECT'; 
	DECLARE @table_name_v nvarchar(256) = N'CovidVaccinations'; 
	
	SELECT @sql_v = @sql_v + ' (SELECT COUNT(*) FROM ' + @table_name_v + ' WHERE ' + QUOTENAME(COLUMN_NAME) + ' IS NULL) as '+ QUOTENAME(COLUMN_NAME) + N',' 
	FROM INFORMATION_SCHEMA.COLUMNS 
	WHERE TABLE_SCHEMA = N'dbo' AND TABLE_NAME = @table_name_v
	SET @sql_v = LEFT(@sql_v, LEN(@sql_v) - 1) + ') as t UNPIVOT (missing_count FOR column_name IN ('; 
	
	SELECT @sql_v = @sql_v + QUOTENAME(COLUMN_NAME) + N',' 
	FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = N'dbo' AND TABLE_NAME = @table_name_v 
	SET @sql_v = LEFT(@sql_v, LEN(@sql_v) - 1) + ')) as u;' 

	EXEC (@sql_v);

	DECLARE @sql_d nvarchar(max) = N'SELECT * FROM (SELECT'; 
	DECLARE @table_name_d nvarchar(256) = N'CovidDeaths'; 
	
	SELECT @sql_d = @sql_d + ' (SELECT COUNT(*) FROM ' + @table_name_d + ' WHERE ' + QUOTENAME(COLUMN_NAME) + ' IS NULL) as '+ QUOTENAME(COLUMN_NAME) + N',' 
	FROM INFORMATION_SCHEMA.COLUMNS 
	WHERE TABLE_SCHEMA = N'dbo' AND TABLE_NAME = @table_name_d
	SET @sql_d = LEFT(@sql_d, LEN(@sql_d) - 1) + ') as t UNPIVOT (missing_count FOR column_name IN ('; 
	
	SELECT @sql_d = @sql_d + QUOTENAME(COLUMN_NAME) + N',' 
	FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = N'dbo' AND TABLE_NAME = @table_name_d 
	SET @sql_d = LEFT(@sql_d, LEN(@sql_d) - 1) + ')) as u;' 

	EXEC (@sql_d);
	--There are missing values in almost all the columns except for the iso_code, location, date, and population columns.
	--This makes sense because at the beginning there are no cases or tests or vaccinations and countries has
	--different ways and time of collecting information.
	
  --Data Cleaning
    
	--The country names contain summarized rows which should not be included in further calculations.
	--So it is better to get rid of these columns first.
	SELECT location, COUNT(DISTINCT location) AS summarized_rows
	FROM CovidVaccinations
	WHERE continent is null
	GROUP BY location;
	
	CREATE VIEW CovidVaccination AS 
	SELECT *
	FROM CovidVaccinations
	WHERE continent IS NOT NULL;

	CREATE VIEW CovidDeath AS 
	SELECT *
	FROM CovidDeaths
	WHERE continent IS NOT NULL;

  --5. Data Analysis
    
	--First, I want to know how many countries are involved and what they are.
	--How many countries are in the dataset?
	SELECT COUNT(DISTINCT location) AS country_count 
	FROM CovidVaccination;
	--The datasets have 243 countries.

	--What are their names?
	SELECT DISTINCT location AS country_name
	FROM CovidVaccination
	ORDER BY country_name;
	
	--Second, I want to measure the mortability and morbidity.
	--What is the total number of cases, deaths, and death rate globally as of the latest date in the dataset?
	SELECT SUM(new_cases) AS total_cases, SUM(new_deaths) AS total_deaths, SUM(new_deaths)/SUM(new_cases)*100 AS death_rate
    FROM CovidDeath;
	--As of the latest date in the dataset, there are totally 772 million people infected by Covid 19, almost 7 million people died.
	--The global death rate is 0.9%.

	--What is the total number of cases, deaths, and death rate for each continents as of the latest date in the dataset?
	SELECT continent, SUM(new_cases) AS total_cases, SUM(new_deaths) AS total_deaths, SUM(new_deaths)/SUM(new_cases)*100 AS death_rate
    FROM CovidDeath
	WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL
	GROUP BY continent
	ORDER BY total_cases DESC;
	ORDER BY total_deaths DESC;
	ORDER BY death_rate DESC;
	--As for continents, Asia has the highest number of cases, Europe has the highest number of deaths, 
	--and Africa has the highest death rate.

	--What is the total number of cases, deaths, and death rate for each country as of the latest date in the dataset?
	SELECT location, SUM(new_cases) AS total_cases, SUM(new_deaths) AS total_deaths, SUM(new_deaths)/SUM(new_cases)*100 AS death_rate
    FROM CovidDeath
	WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL
	GROUP BY location
	ORDER BY total_cases DESC;
	ORDER BY total_deaths DESC;
	ORDER BY death_rate DESC;
	--As of the latest date in the dataset, the United States has the highest number of cases, about 103 million people affected.
	--The countries rating second and third are China and India, with about 99 million and 45 million respectively.
	--The United States still ranks number one over number of deaths, about 1 million people.
	--Brazil and India rated second and third, with 70 thousands and 53 thousands respectively.
	--Yemen has the highest death rate of about 18%, more than doubling the number of the second country, which is Sudan.
	
	--How many countries has above average death rate?
	WITH cte AS (
		SELECT location, SUM(new_cases) AS total_cases, SUM(new_deaths) AS total_deaths, SUM(new_deaths)/SUM(new_cases)*100 AS death_rate
		FROM CovidDeath
		WHERE total_cases IS NOT NULL AND total_deaths IS NOT NULL
		GROUP BY location
	)
	SELECT COUNT(*) AS country_count
	FROM cte
	WHERE death_rate > (
		SELECT AVG(death_rate)
		FROM cte
		);
	--Out of the 243 countries, almost one third of them, about 70 countries, have death rates higher than the global average.

	--How does the excess mortality rate vary by month and year across different continents?
	--The excess mortality rate is the percentage of deaths above the expected level based on the historical average. 
	--It can be used as an indicator of the impact of the COVID 19 on the mortality. 
	SELECT YEAR(date) AS [year], MONTH(date) AS [month], continent, AVG(CAST(excess_mortality AS float)) AS avg_exc_mort
	FROM CovidVaccination
	GROUP BY YEAR(date), MONTH(date), continent
	ORDER BY avg_exc_mort DESC;
	--The highest excess mortality rates occur in South America, North America, and Africa, 
	--whereas Asia, Europe, and Oceania have relatively lower rates.
	--This may indicate that Asia and Europe have deaths spreading out over the three years 
	--while the number of deaths occur in the other three continents may rise suddenly during a certain time period.

	--Third, I want to measure the testing and vaccination activities.
	--What is the total number of tests, vaccinations, and boosters done globally as of the latest date in the dataset?
	SELECT MAX(total_tests) AS total_tests, MAX(total_boosters) AS total_boosters, MAX(total_vaccinations) AS total_vaccinations
	FROM CovidVaccination;
	--As of the latest date in the dataset, about 9.2 billion tests were conducted, about 3.5 billion vaccinations 
	--and 827 million boosters were taken around the world.

	--How many countries has above average vaccination rate?
	CREATE VIEW vaccinated AS 
		SELECT location, MAX(people_fully_vaccinated) AS people_fully_vaccinated, MAX(date) AS latest_date
		FROM CovidVaccination
		WHERE people_fully_vaccinated IS NOT NULL
		GROUP BY location;
	WITH cte AS (
		SELECT v.location, people_fully_vaccinated*100/population AS vaccination_rate
		FROM CovidDeath cd
		RIGHT JOIN vaccinated v
		ON cd.location = v.location AND cd.date = v.latest_date
		)
	SELECT location, vaccination_rate
	FROM cte
	WHERE vaccination_rate > (SELECT AVG(vaccination_rate) FROM cte)
	ORDER BY vaccination_rate DESC;
	--Until the latest date in the dataset, almost half of the countries has above average vaccination rate.
		
	--rolling vaccinations vs. total population, which shows how the vaccinations are conducted in each country.
	SELECT d.continent, d.location, d.date, population, new_vaccinations,
	SUM(new_vaccinations) OVER (PARTITION BY d.location ORDER BY d.location, d.date) AS rolling_vaccinated
	FROM CovidDeaths d
	JOIN CovidVaccinations v
	ON d.location = v.location AND d.date = v.date
	WHERE d.continent IS NOT NULL
	ORDER BY 2, 3;

	--Next, I want to identify factors that may influence COVID-19 outcomes.
	--total cases vs. population for each country, which shows the percentage of population got Covid
	WITH cte_cases AS (
		SELECT iso_code, MAX(total_cases) AS total_cases
		FROM CovidDeath
		GROUP BY iso_code
		),
		cte_date AS (
		SELECT iso_code, MAX(date) AS latest_date
		FROM CovidDeath
		GROUP BY iso_code
		)
	SELECT cd.location, date, c.total_cases, population, (c.total_cases/population)*100 AS infection_rate
	FROM CovidDeath cd
	JOIN cte_cases c
	ON cd.iso_code = c.iso_code 
	JOIN cte_date d
	ON cd.iso_code = d.iso_code
	WHERE cd.date = d.latest_date
	ORDER BY infection_rate DESC;
	--The countries with the highest infection rate are those with small populations.

	--Which countries have more than 50% of their population fully vaccinated and how much of their population living in extreme poverty?
	SELECT location, MAX(people_fully_vaccinated_per_hundred) as max_fully_vaccinated, CAST(extreme_poverty AS float) as extreme_poverty
	FROM CovidVaccination
	WHERE people_fully_vaccinated_per_hundred > 50 AND extreme_poverty IS NOT NULL
	GROUP BY location, extreme_poverty
	ORDER BY max_fully_vaccinated DESC;
	--Seems proverty does not directly related to vaccination.

	--the percentage of female smokers and male smokers among the locations that have the highest and lowest total deaths per million from COVID-19
	CREATE VIEW death_per_million AS
		SELECT cv.location, cv.date, total_deaths_per_million, 
			   CAST(female_smokers AS float) AS female_smokers, 
			   CAST(male_smokers AS float) AS male_smokers
		FROM CovidVaccination cv
		JOIN CovidDeath cd
		ON cv.iso_code = cd.iso_code AND cv.date = cd.date
		WHERE total_deaths_per_million IS NOT NULL
			  AND female_smokers IS NOT NULL
			  AND male_smokers IS NOT NULL;
	CREATE VIEW last_date AS
		SELECT location, MAX(date) AS last_date
		FROM death_per_million	
		GROUP BY location;
	WITH cte_rank AS (
		SELECT m.location, d.last_date, female_smokers, male_smokers, total_deaths_per_million,
			   RANK() OVER (ORDER BY total_deaths_per_million DESC) AS death_rank
		FROM death_per_million m
		RIGHT JOIN last_date d
		ON m.location = d.location AND m.date = d.last_date
		)
	SELECT location, last_date, female_smokers, male_smokers, total_deaths_per_million
	FROM cte_rank
	WHERE death_rank = 1 OR death_rank = (SELECT MAX(death_rank) FROM cte_rank)
	ORDER BY death_rank;
	--In the country with the highest total deaths per million, almost one third of women smoke and almost half of men smoke,
	--whereas the lowest country has only 0.1% of women and 15% men smoking. The differences are very big.
	--Seems smoking has a great impact on dying from COVID 19.

	--Finally, I want to measure the effectiveness of vaccinations and boosters.
	--What are the top 10 countries with the highest ratio of people fully vaccinated to people tested positive for COVID-19?
	CREATE VIEW ratio AS
		SELECT location, date, people_fully_vaccinated/(total_tests*positive_rate) AS ratio
		FROM CovidVaccination
		WHERE total_tests <> 0 AND positive_rate <> 0;
	WITH latest_date AS (
		SELECT location, MAX(date) as latest_date
		FROM ratio
		WHERE ratio IS NOT NULL
		GROUP BY location
		)
	SELECT TOP 10 d.location, d.latest_date, r.ratio 
	FROM ratio r
	RIGHT JOIN latest_date d
	ON r.location = d.location AND r.date = d.latest_date
	ORDER BY r.ratio DESC;
	--The higher the ratio, the more effective the vaccinations to the country.
	--Countries with the highest effectiveness are some small countries. 

  --6. Conclusion

	--The COVID 19 has a very negative impact to the world, causing millions of deaths and billions of infections.
	--Almost one third of the world's coutries has above average death rate.
	--Fortunately, countries were taking positive actions. 
	--Until the latest date in the dataset, almost half of the countries has above average vaccination rate.
	--More insights can be generated from the dataset. This project is a small portion and a demo for the insights.




