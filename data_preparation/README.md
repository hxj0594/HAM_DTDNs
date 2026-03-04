Country Name Cross-Data
INPUT: Artificial Download

OUTPUT: WB.country.rank.190.csv

Details:

Get country code from https://countrycode.org/
Get WBCountry from https://data.worldbank.org/country
Get WBCountry Code、Region、Income group from http://databank.worldbank.org/data/download/site-content/CLASS.xlsx
Get country name from http://ghdx.healthdata.org/gbd-results-tool




Education
Data source Barro Lee education database (from 1990-2040):

http://www.barrolee.com




Labour data
Data source ILOSTAT International Labour Organization Statistics (from 2010-2030)

https://ilostat.ilo.org/data/




Population Data
Population Data Source: Department of Economic and Social Affairs Population Dynamics

United Nations Population Website

Downloads:

Total Population from 1950 to 2100. https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv

Population by Age and Sex from 1950 to 2100. https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv





Savings data 
Data Source: The world bank, assume savings rate going forward is the average between 2010-2019

Gross Savings of GDP

https://data.worldbank.org/indicator/NY.GNS.ICTR.ZS

save as sav.rate.124.csv








Health expenditure 
Data Source: The world bank,

Current health expenditure (% of GDP)

https://api.worldbank.org/v2/en/indicator/SH.XPD.CHEX.GD.ZS?downloadformat=csv https://data.worldbank.org/indicator/SH.XPD.CHEX.GD.ZS

Current health expenditure per capita, PPP (current international $)

https://api.worldbank.org/v2/en/indicator/SH.XPD.CHEX.PP.CD?downloadformat=csv











Mortality and Morbidity
Mortality and morbidity data source: Global Health Data Exchange





Physical CapitalData Source: Penn World Table (from 1990-2019)
save as kt.dt.all.csv



Treatment Cost
In our study, total treatment costs for each DTDNs in the United States are based on results from Dieleman et al. (2020), who systematically estimated national-level spending on private insurance, public insurance, and out-of-pocket payments for different conditions after considering comorbidities. Their spending estimates ($141.4 billion) amounted to 4.40% of the total health expenditures in the United States that year. Supplementary Table 2 shows the treatment costs for 16 DTDNs in the United States.

We extrapolated treatment costs for countries without DTDNs treatment cost data, assuming that the per case treatment cost for DTDNs was proportional to the health expenditure per capita of the country, consistent with previous studies. Based on this assumption, the share of DTDNs-related treatment costs out of all health expenditures can be calculated using DTDNs prevalence rates. Specifically, for each type of DTDNs, we estimated that a country's ratio of treatment cost to total health expenditure, relative to that of the United States, equals the ratio of DTDN prevalence rates between that country and the United States. For years after 2010, we assumed that DTDNs treatment costs grow at the same rate as per capita health expenditures for each country.

