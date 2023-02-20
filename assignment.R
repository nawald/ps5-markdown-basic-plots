### Gapminder data
## We use gapminder dataset, downloaded from https://www.gapminder.org/data/, however, the
## data structure there is quire complex, please use the dataset provided on canvas (in files/data).
## The variables are:
##*name**: country name
##*iso3*: 3-letter country code
##*iso2*: 2-letter country code
##*region**: broad geographic region
##*sub-region**: more precise region
##*intermediate-region**
##*time** year
##*totalPopulation**: total population
##*GDP_PC**: GDP per capita (constant 2010 US$)
##*accessElectricity**: Access to electricity (% of population)
##*agriculturalLand**: Agricultural land (sq. km)
##*agricultureTractors**: Agricultural machinery, tractors (count)
##*cerealProduction**: Cereal production (metric tons)
##*feritilizerHa**: Fertilizer consumption (kilograms per hectare of arable land)
##*fertilityRate**: total fertility rate (births per woman)
##*lifeExpectancy**: Life expectancy at birth, total (years)
##*childMortality**: Mortality rate, under-5 (per 1,000 live births)
##*youthFemaleLiteracy**: Literacy rate, youth female (% of females ages 15-24)
##*youthMaleLiteracy**: Literacy rate, youth male (% of males ages 15-24)
##*adultLiteracy**: Literacy rate, adult total (% of people ages 15 and above)
##*co2**: CO2 emissions (kt)
##*greenhouseGases**: Total greenhouse gas emissions (kt of CO2 equivalent)
##*co2_PC**: CO2 emissions (metric tons per capita)
##*pm2.5_35**: PM2.5 pollution, population exposed to levels exceeding WHO Interim Target-1 value
  ##36ug/m3 (
##*battleDeaths**: Battle-related deaths (number of people)


##1 Load and check data (5pt)
##You first task is to do a very simple data check:
##1. (1pt) For solving the problems, and answering the questions, create a new rmarkdown docu-
  ##ment with an appropriate title. See https://faculty.washington.edu/otoomet/info201-book/
  ##r-markdown.html#r-markdown-rstudio-creating.

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
##2. (2pt) Load data. How many rows/columns do we have?

##3. (2pt) Print a small sample of data. Does it look OK?


##2 Descriptive statistics (15pt)
##1. (3pt) How many countries are there in the dataset? Analyze all three: iso3, iso2 and name.

#iso3 (3 letter country code) = 253 distinct values
gapminder %>% 
  select(iso3) %>% 
  filter(!is.na(iso3)) %>% 
  n_distinct()
  
#iso2 (2 letter country code) = 248 distinct values
gapminder %>% 
  select(iso2) %>% 
  filter(!is.na(iso2)) %>% 
  n_distinct()  

#name = 249 distinct values
gapminder %>% 
  select(name) %>% 
  filter(!is.na(name)) %>% 
  n_distinct() 
  

##2. If you did this correctly, you saw that there are more names than iso-2 codes, and there are
  ##even more iso3 -codes. What is going on? Can you find it out?
##(a) (5pt) Find how many names are there for each iso-2 code. Are there any iso-2 codes that
    ##correspond to more than one name? What are these countries?
find_out <- gapminder %>% 
  group_by(iso2) %>% 
  summarize(total = n_distinct(name)) %>% 
  arrange(-total)
which <- gapminder %>% 
  select(iso2, name) %>% 
  filter(is.na(iso2)) %>% 
  distinct()

##(b) (5pt) Now repeat the same for name and iso3-code. Are there country names that have
  ##more than one iso3-code? What are these countries?
  ##Hint: two of these entitites are CHANISL and NLD CURACAO.

thing <- gapminder %>% 
  group_by(name) %>% 
  summarize(total = n_distinct(iso3)) %>% 
  arrange(-total)

another <- gapminder %>% 
  select(iso3, name) %>% 
  filter(is.na(name)) %>% 
  distinct()

##3. (2pt) What is the minimum and maximum year in these data?
numbers <- gapminder %>% 
  filter(!is.na(time)) 
  
mini <- min(numbers$time)
mini

maxi <- max(numbers$time)
maxi
  
##3 CO2 emissions (30pt)
##Next, let’s analyze CO2 emissions.
##1. (2pt) How many missing co2 emissions are there for each year? Analyze both missing CO2
  ## and co2_PC. Which years have most missing data?

#co2
missing_co2 <- gapminder %>% 
  filter(is.na(co2)) %>% 
  filter(!is.na(time)) %>% 
  group_by(time) %>% 
  filter(time %in% (1960:2019)) %>% 
  summarize(
    missing_co2_values = n()
  ) %>% 
  filter(rank(desc(missing_co2_values)) < 4) %>% 
  knitr::kable()

#co2_PC
missing_co2_PC <- gapminder %>% 
  filter(is.na(co2_PC)) %>% 
  filter(!is.na(time)) %>% 
  group_by(time) %>% 
  filter(time %in% (1960:2019)) %>% 
  summarize(
    missing_co2_PC_values = n()
  ) %>% 
  filter(rank(desc(missing_co2_PC_values)) < 4) %>% 
  knitr::kable()



##2. (5pt) Make a plot of total CO2 emissions over time for the U.S, China, and India. Add a few
  ##more countries of your choice. Explain what do you see.
    emissions <- gapminder %>% 
      group_by(name, time) %>% 
      filter(!is.na(co2)) %>% 
      filter(!is.na(time)) %>% 
      filter(name == 'United States of America'| name == 'China' | name == 'India'
             | name == 'Aruba' | name == 'Afghanistan') %>% 
      summarize(
        total_co2 = sum(co2)
      )
    
    ggplot(emissions, aes(x = time, y = total_co2, color = name)) +
      geom_line() +
      ggtitle('Total co2 emissions over time')
      

##3. (5pt) Now let’s analyze the CO2 emissions per capita (co2_PC ). Make a similar plot of the
  ##same countries. What does this figure suggest?
    
    
    capita <- gapminder %>% 
      group_by(name, time) %>% 
      filter(!is.na(co2_PC)) %>% 
      filter(!is.na(time)) %>% 
      filter(name == 'United States of America'| name == 'China' | name == 'India'
             | name == 'Aruba' | name == 'Afghanistan') %>% 
      
      summarize(
        total_emissions = sum(co2_PC)
      )
    
    ggplot(capita, aes(x = time, y = total_emissions, color = name)) +
      geom_line() +
      ggtitle('Total co2 emissions per capita')
    
##4. (6pt) Compute average CO2 emissions per capita across the continents (assume region is the
  ##same as continent). Comment what do you see.
##Note: just compute averages over countries and ignore the fact that countries are of different
  ##size.
##Hint: Americas 2016 should be 4.80.
    average <- gapminder %>% 
      filter(!is.na(co2_PC),!is.na(region)) %>% 
      group_by(region, time) %>% 
      summarize(
        total = mean(co2_PC)
      )
    
##5. (7pt) Make a barplot where you show the previous results–average CO2 emissions per capita
  ##across continents in 1960 and 2016.
    average <- gapminder %>% 
      filter(!is.na(co2_PC),!is.na(region)) %>% 
      filter(time == '1960'| time == '2016') %>% 
      group_by(region, time) %>% 
      mutate(avg_co2_PC = mean(co2_PC)) 
    
    ggplot(average, aes(region, avg_co2_PC, fill = factor(time))) +
    geom_bar(stat = 'identity', position = 'dodge')
    
##6. Which countries are the three largest, and three smallest CO2 emitters (in terms of CO2 per
  ##capita) in 2019 for each continent? (Assume region is continent).
#Since 2019 doesn't have enough data, Ill use 2016
    
##Americas: Top 3 = Trinidad & Tobago, United States of America, Canada
            #Bottom 3 = Honduras, Nicaragua, Haiti
    
##Asia: Top 3 = Qatar, Kuwait, Bahrain
        #Bottom 3 = Yemen, Nepal, Afghanistan
    
##Europe: Top 3 = Gibraltar, Luxembourg, Faroe Islands
      #Bottom 3 = Moldova, Republic Of, Albania, Liechtenstein
    
#Africa: Top 3 = South Africa, Libya, Seychelles
    #Bottom 3 = Congo, Democratic Republic Of, Somalia, Burundi
    
#Oceania: Top 3 = New Caledonia, Australia, Palau
    #Bottom 3 = Kiribati, Vanuatu, Solomon Islands
    
    three <-  gapminder %>% 
  select(name, region, time, co2_PC) %>% 
  filter(!is.na(co2_PC), !is.na(name), !is.na(region), !is.na(time)) %>% 
  filter(time == '2016') %>% 
  group_by(region, name, time) %>% 
  summarize(
    total = sum(co2_PC)
  ) %>% 
    arrange(-total)
    
  three_highest <- three %>% 
    group_by(region) %>% 
    filter(rank(desc(total)) < 4)
  
  three_lowest <- three %>% 
    group_by(region) %>% 
    filter(rank(desc(-total)) < 4)

##4 GDP per capita (50pt)

##Let’s look at GDP per capita (GDP_PC ).
##1. (8pt) Make a scatterplot of GDP per capita versus life expectancy by country, using data for
  ##1960. Make the point size dependent on the country size, and color those according to the
  ##continent. Feel free to adjust the plot in other ways to make it better.
  ##Comment what do you see there.
  gdp <- gapminder %>% 
    filter(!is.na(name), !is.na(region), !is.na(GDP_PC), !is.na(time), 
           !is.na(lifeExpectancy), !is.na(totalPopulation)) %>% 
    filter(time == '1960') %>% 
    group_by(name, region, lifeExpectancy, GDP_PC, totalPopulation) %>% 
    select(name, region, time, lifeExpectancy, GDP_PC, totalPopulation)
  
  ggplot(gdp,
         aes(GDP_PC, lifeExpectancy, col=region)) +
    geom_point(aes(size=totalPopulation)) +
    labs( x = 'GDP per capita',
          y = 'Life Expectancy') +
    ggtitle('GDP per capita versus life expectancy by country in 1960')

##2. (4pt) Make a similar plot, but this time use 2019 data only.
  gdp2 <- gapminder %>% 
    filter(!is.na(name), !is.na(region), !is.na(GDP_PC), !is.na(time), 
           !is.na(lifeExpectancy), !is.na(totalPopulation)) %>% 
    filter(time == '2019') %>% 
    group_by(name, region, lifeExpectancy, GDP_PC, totalPopulation) %>% 
    select(name, region, time, lifeExpectancy, GDP_PC, totalPopulation)
  
  ggplot(gdp2,
         aes(GDP_PC, lifeExpectancy, col=region)) +
    geom_point(aes(size=totalPopulation)) +
    labs( x = 'GDP per capita',
          y = 'Life Expectancy') +
    ggtitle('GDP per capita versus life expectancy by country in 2019')
  
##3. (6pt) Compare these two plots and comment what do you see. How has world developed
  ##through the last 60 years?
#~~~DONE
  
##4. (6pt) Compute the average life expectancy for each continent in 1960 and 2019. Do the results
  ##fit with what do you see on the figures?
  ##Note: here as average I mean just average over countries, ignore the fact that countries are of
  ##different size.
  average_le_2019 <- gapminder %>% 
    filter(!is.na(lifeExpectancy),!is.na(region), !is.na(name), !is.na(time)) %>% 
    filter(time == '2019') %>% 
    group_by(region, time) %>% 
    summarize(
      total_le = mean(lifeExpectancy)
    ) 
  
  average_le_1960 <- gapminder %>% 
    filter(!is.na(lifeExpectancy),!is.na(region), !is.na(name), !is.na(time)) %>% 
    filter(time == '1960') %>% 
    group_by(region, time) %>% 
    summarize(
      total = mean(lifeExpectancy)
    ) 
  
##5. (8pt) Compute the average LE growth from 1960-2019 across the continents. Show the results
  ##in the order of growth. Explain what do you see.
  ##Hint: these data (data in long form) is not the simplest to compute growth. But you may
  ##want to check out the lag() function. And do not forget to group data by continent when
  ##using lag(), otherwise your results will be messed up! See https://faculty.washington.
  ##edu/otoomet/info201-book/dplyr.html#dplyr-helpers-compute.
  average_growth <- gapminder %>% 
    filter(!is.na(lifeExpectancy),!is.na(region), !is.na(name), !is.na(time)) %>% 
    filter(time == '1960'| time == '2019') %>% 
    group_by(region, time) %>% 
    summarize(
      le_average = mean(lifeExpectancy)
    )  %>% 
    mutate(previous_year = lag(le_average), growth = le_average - previous_year) %>% 
    filter(!is.na(growth)) %>% 
    arrange(-growth)

##6. (6pt) Show the histogram of GDP per capita for years of 1960 and 2019. Try to put both
##histograms on the same graph, see how well you can do it!
  history <- gapminder %>% 
    filter(!is.na(time), !is.na(GDP_PC)) %>% 
    filter(time == '1960'| time == '2019') %>% 
    group_by(time, GDP_PC) %>% 
    summarize(
      GDP_capita = sum(GDP_PC)
    ) 
  
  ggplot(history, aes(x = GDP_PC)) +
    geom_histogram(color = 'purple', alpha=0.5, position = 'identity') +
    facet_wrap(~time)

##7. (6pt) What was the ranking of US in terms of life expectancy in 1960 and in 2019? (When
##counting from top.)
  ##Hint: check out the function rank()!
  ##Hint2: 17 for 1960.
  
  rank_1960 <- gapminder %>% 
    filter(!is.na(time), !is.na(name), !is.na(lifeExpectancy)) %>% 
    filter(time == '1960') %>% 
    group_by(time, name, lifeExpectancy) %>% 
    arrange(-lifeExpectancy)
  
  rank_2019 <- gapminder %>% 
    filter(!is.na(time), !is.na(name), !is.na(lifeExpectancy)) %>% 
    filter(time == '2019') %>% 
    group_by(time, name, lifeExpectancy) %>% 
    arrange(-lifeExpectancy)

  #1960: Rank 17th highest
  #2019: Rank 46th Highest
  
##8. (6pt) If you did this correctly, then you noticed that US ranking has been falling quite a
  ##bit. But we also have more countries in 2019–what about the relative rank divided by the
  ##corresponding number of countries that have LE data in the corresponding year?
  ##Hint: 0.0904 for 1960.
 
  total_countries <- length(rank_1960$name)
  
  filler <- rank_1960 %>% 
    mutate(ranking = rank(desc(lifeExpectancy)),
           ranked = ranking/total_countries) %>% 
    select(name, ranking, ranked) %>% 
    filter(name == 'United States of America')
  
  

  
  

##Finally tell us how many hours did you spend on this PS.
