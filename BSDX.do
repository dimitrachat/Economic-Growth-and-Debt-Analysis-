/* replace string characters in the variable that are not appropriate for stata in order to understand them as missing values*/

replace countryname="." if countryname==".."
replace fuelexportsofmerchandiseexportst ="." if fuelexportsofmerchandiseexportst ==".."
replace fuelimportsofmerchandiseimportst ="." if fuelimportsofmerchandiseimportst ==".."
replace gdpgrowthannualnygdpmktpkdzg="." if gdpgrowthannualnygdpmktpkdzg==".."
replace laborforcetotalsltlftotlin="." if laborforcetotalsltlftotlin==".."
replace tradeofgdpnetrdgnfszs="." if tradeofgdpnetrdgnfszs==".."
replace taxrevenueofgdpgctaxtotlgdzs="." if taxrevenueofgdpgctaxtotlgdzs==".."
replace grossfixedcapitalformationofgdpn="." if grossfixedcapitalformationofgdpn==".."
replace generalgovernmentfinalconsumptio="." if generalgovernmentfinalconsumptio==".."
replace realinterestratefrinrrinr="." if realinterestratefrinrrinr==".."
replace foreigndirectinvestmentnetinflow="." if foreigndirectinvestmentnetinflow==".."
replace foreigndirectinvestmentnetoutflo="." if foreigndirectinvestmentnetoutflo==".."
replace unemploymenttotaloftotallaborfor="." if unemploymenttotaloftotallaborfor==".."
replace inflationgdpdeflatorannualnygdpd="." if inflationgdpdeflatorannualnygdpd==".."
replace suicidemortalityrateper100000pop="." if suicidemortalityrateper100000pop==".."
replace currentaccountbalanceofgdpbncabx="." if currentaccountbalanceofgdpbncabx==".."
 

/* Now we have the right form to convert the variables from string type to numeric type */
 
*DESTRING AND RENAME THE VARIABLES

encode countryname, gen (country) 
rename time year //was already numeric 
rename timecode yearcode 
rename countrycode ccode
rename generalgovermentdebt debt //was already numeric
destring fuelexportsofmerchandiseexportst, replace
destring fuelimportsofmerchandiseimportst,replace 
destring gdpgrowthannualnygdpmktpkdzg,replace
destring laborforcetotalsltlftotlin,replace
destring tradeofgdpnetrdgnfszs,replace 
destring taxrevenueofgdpgctaxtotlgdzs,replace 
destring grossfixedcapitalformationofgdpn,replace
destring generalgovernmentfinalconsumptio,replace
destring realinterestratefrinrrinr,replace
destring foreigndirectinvestmentnetinflow,replace
destring foreigndirectinvestmentnetoutflo,replace
destring unemploymenttotaloftotallaborfor,replace
destring inflationgdpdeflatorannualnygdpd,replace
destring suicidemortalityrateper100000pop,replace
destring currentaccountbalanceofgdpbncabx,replace


*SET THE DATA TO PANEL FORMAT

xtset country year



* MANAGING DATA

//MISSING DATA:

sort country year
bysort country:ipolate gdpgrowthannualnygdpmktpkdzg year, gen (GDP_growth_full) epolate
bysort country:ipolate fuelexportsofmerchandiseexportst year, gen (fuel_exports) epolate 
bysort country:ipolate fuelimportsofmerchandiseimportst year, gen (fuel_imports) epolate 
bysort country:ipolate tradeofgdpnetrdgnfszs year, gen (trade_full) epolate 
bysort country:ipolate taxrevenueofgdpgctaxtotlgdzs year , gen (tax_rev_full) epolate 
bysort country:ipolate grossfixedcapitalformationofgdpn year, gen (gross_fixed_cap_form_full) epolate
bysort country:ipolate laborforcetotalsltlftotlin year, gen (labor_full) epolate
bysort country:ipolate generalgovernmentfinalconsumptio year, gen (Govern__consumption_full) epolate 
bysort country:ipolate realinterestratefrinrrinr year, gen (real_interest_rate_full) epolate 
bysort country:ipolate foreigndirectinvestmentnetinflow year,gen (foreign_invest_inflow_full) epolate 
bysort country:ipolate foreigndirectinvestmentnetoutflo year,gen (foreign_invest_outflow_full) epolate
bysort country:ipolate unemploymenttotaloftotallaborfor year, gen (unemployment_full) epolate
bysort country:ipolate inflationgdpdeflatorannualnygdpd year, gen (inflation_full) epolate
bysort country:ipolate suicidemortalityrateper100000pop year, gen (suicide_full) epolate
bysort country:ipolate currentaccountbalanceofgdpbncabx year, gen (account_balance_full) epolate
bysort country:ipolate debt year, gen (debt_full) epolate
bysort country:ipolate lifeexpectancyatbirthtotalyearss year, gen (life_expectancy) epolate
bysort country:ipolate populationgrowthannualsppopgrow year, gen (population_growth) epolate 


//DELETE PREVIOUS UNNECESSARY VARIABLES

drop fuelexportsofmerchandiseexportst fuelimportsofmerchandiseimportst tradeofgdpnetrdgnfszs taxrevenueofgdpgctaxtotlgdzs gdpgrowthannualnygdpmktpkdzg grossfixedcapitalformationofgdpn laborforcetotalsltlftotlin generalgovernmentfinalconsumptio realinterestratefrinrrinr foreigndirectinvestmentnetinflow foreigndirectinvestmentnetoutflo unemploymenttotaloftotallaborfor inflationgdpdeflatorannualnygdpd suicidemortalityrateper100000pop countryname currentaccountbalanceofgdpbncabx



* GENERATE A TIME VARIABLE FOR EACH COUNTRY:

bysort country: gen time = _n



//GENERATE (ANNUAL) GROWTH RATES OF MY VARIABLES

//Growth of labor force

bysort country: gen loglabor = ln(labor_full)
sort country year
by country: gen growth_labor= loglabor - loglabor[_n-1]



* GRAPHICAL ANALYSIS

//HISTOGRAMS

histogram GDP_growth_full
histogram GDP_growth_full , kdensity normal
histogram GDP_growth_full , kdensity normal by (country, total)


histogram trade_full
histogram trade_full, kdensity normal
histogram trade_full , kdensity normal by (country, total)


histogram debt_full
histogram debt_full, kdensity normal
histogram debt_full, kdensity normal by (country, total)


histogram growth_labor
histogram growth_labor , kdensity normal
histogram growth_labor , kdensity normal by (country, total)


histogram gross_fixed_cap_form_full
histogram gross_fixed_cap_form_full, kdensity normal 
histogram gross_fixed_cap_form_full , kdensity normal by (country, total)


histogram tax_rev_full
histogram tax_rev_full , kdensity normal 
histogram tax_rev_full , kdensity normal by (country, total)


histogram Govern__consumption_full
histogram Govern__consumption_full, kdensity normal
histogram Govern__consumption_full, kdensity normal by ( country,total)


histogram real_interest_rate_full
histogram real_interest_rate_full, kdensity normal
histogram real_interest_rate_full, kdensity normal by ( country,total)


histogram foreign_invest_inflow_full
histogram foreign_invest_inflow_full , kdensity normal
histogram foreign_invest_inflow_full , kdensity normal by ( country,total)


histogram foreign_invest_outflow_full
histogram foreign_invest_outflow_full, kdensity normal
histogram foreign_invest_outflow_full, kdensity normal by ( country,total)


histogram unemployment_full, kdensity normal by ( country,total)
histogram unemployment_full
histogram unemployment_full, kdensity normal


histogram suicide_full
histogram suicide_full, kdensity normal
histogram suicide_full, kdensity normal by ( country,total)


histogram account_balance_full, kdensity normal by ( country,total)
histogram account_balance_full
histogram account_balance_full, kdensity normal


histogram inflation_full
histogram inflation_full, kdensity normal
histogram inflation_full, kdensity normal by ( country,total)


histogram life_expectancy
histogram life_expectancy, kdensity normal
histogram life_expectancy, kdensity normal by ( country,total)


histogram population_growth
histogram population_growth, kdensity normal
histogram population_growth, kdensity normal by ( country,total)



//BOX PLOTS:

graph box GDP_growth_full, by( country, total)
graph box trade_full, by( country, total)
graph box debt_full, by(country, total)
graph box growth_labor, by(country, total)
graph box gross_fixed_cap_form_full, by(country, total)
graph box tax_rev_full, by( country, total)
graph box Govern__consumption_full, by(country, total)
graph box real_interest_rate_full, by(country, total)
graph box foreign_invest_inflow_full , by(country, total)
graph box foreign_invest_outflow_full , by(country, total)
graph box unemployment_full, by(country, total)
graph box suicide_full , by(country, total)
graph box account_balance_full, by(country, total) 
graph box inflation_full, by(country, total)
graph box life_expectancy, by(country, total)
graph box population_growth , by(country, total)



//SCATTER PLOTS:

twoway scatter GDP_growth_full debt_full
twoway scatter GDP_growth_full growth_labor
twoway scatter GDP_growth_full trade_full
twoway scatter GDP_growth_full gross_fixed_cap_form_full
twoway scatter GDP_growth_full tax_rev_full 
twoway scatter GDP_growth_full Govern__consumption_full
twoway scatter GDP_growth_full real_interest_rate_full
twoway scatter GDP_growth_full foreign_invest_inflow_full
twoway scatter GDP_growth_full foreign_invest_outflow_full
twoway scatter GDP_growth_full life_expectancy
twoway scatter GDP_growth_full inflation_full
twoway scatter GDP_growth_full suicide_full
twoway scatter GDP_growth_full account_balance_full
twoway scatter GDP_growth_full population_growth
twoway scatter GDP_growth_full unemployment_full



graph matrix GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth



*DESCRIPTIVE STATISTICS

xtdescribe

sum GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth



//information on the variation of each variable

xtsum GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth



*REGRESSION ANALYSIS


//POOLED ESTIMATORS 


//POOLED OLS

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth



//POOLED OLS with CLUSTER-ROBUST standard errors estimator:


//OLS ROBUST  /* for unclustered data */ /*(WLS)*/

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth, vce(robust)



*PREDICTION:

predict GDP_growth_full_hat
predict uhat, res

pwcorr GDP_growth_full GDP_growth_full_hat,star(0.05) sig



* (MIS)SPECIFICATION TEST(S)

//(1) RESET TEST 

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

estat ovtest 



//(2) OMNIBUS TEST

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

estat imtest



//(3) NORMALITY OF ERRORS

kdensity uhat, normal
	
	

//STANDARDIZE NORMAL PROBAABILITY PLOT

pnorm uhat



//QUINTILE-NORMAL PLOTS

qnorm uhat



//SHAPIRO-WILK TEST FOR NORMALITY

swilk uhat



//Heteroskedasticity (graphically)

predict duhat, r
gen square_residuals = duhat^2
twoway (scatter square_residuals  GDP_growth_full_hat )
twoway (scatter square_residuals trade_full)
twoway (scatter square_residuals tax_rev_full )
twoway (scatter square_residuals gross_fixed_cap_form_full )
twoway (scatter square_residuals Govern__consumption_full )
twoway (scatter square_residuals real_interest_rate_full )
twoway (scatter square_residuals foreign_invest_inflow_full )

twoway (scatter square_residuals unemployment_full )
twoway (scatter square_residuals inflation_full )
twoway (scatter square_residuals suicide_full )
twoway (scatter square_residuals account_balance_full )
twoway (scatter square_residuals debt_full )
twoway (scatter square_residuals life_expectancy )
twoway (scatter square_residuals population_growth )
twoway (scatter square_residuals growth_labor )



* TEST: HETEROSCEDASTICITY

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

estat hettest
 
 
 
*CORRECT HETEROSCEDASTICITY //WLS

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth, vce(robust)



//correlation among variables (with significance level) //1st Statistical way for detection of multicollinearity:

pwcorr GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,star(0.05) sig



* TEST: MULTICOLLINEARITY //2nd statistical way for detection of multicollinearity:
//Graphically 

graph matrix  trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth
**

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

vif



*TEST AUTOCORRELATION 

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

predict resid, resid

sort country year
gen lagresi = resid[_n-1]
reg resid lagresi

sort country year
gen l_growth = GDP_growth_full[_n-1]
reg GDP_growth_full l_growth trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

//install xtserial

findit xtserial

net sj 3-2 st0039  

net install st0039  


//detect autocorrelation

xtserial GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth



**Fix autocorrelation (First Differences)

gen dGDP_growth_full= d.GDP_growth_full
gen dtrade_full= d.trade_full
gen dtax_rev_full= d.tax_rev_full
gen dgross_fixed_cap_form_full= d.gross_fixed_cap_form
gen dgrowth_labor= d.growth_labor 
gen dGovern_consumption_full= d.Govern__consumption_full
gen dreal_interest_rate_full= d.real_interest_rate_full 
gen dforeign_invest_inflow_full= d.foreign_invest_inflow_full 
gen dforeign_invest_outflow_full= d.foreign_invest_outflow_full 
gen dunemployment_full= d.unemployment_full 
gen dinflation_full= d.inflation_full 
gen dsuicide_full= d.suicide_full 
gen daccount_balance_full= d.account_balance_full 
gen ddebt_full= d.debt_full 
gen dlife_expectancy= d.life_expectancy 
gen dpopulation_growth= d.population_growth


reg dGDP_growth_full dtrade_full dtax_rev_full dgross_fixed_cap_form_full dgrowth_labor dGovern_consumption_full dreal_interest_rate_full dforeign_invest_inflow_full dforeign_invest_outflow_full dunemployment_full dinflation_full dsuicide_full daccount_balance_full ddebt_full dlife_expectancy dpopulation_growth

gen lr = duhat[_n-1]

reg duhat lr


//test again to see if autocorrelation is fixed

xtserial dGDP_growth_full dtrade_full dtax_rev_full dgross_fixed_cap_form_full dgrowth_labor dGovern_consumption_full dreal_interest_rate_full dforeign_invest_inflow_full dforeign_invest_outflow_full dunemployment_full dinflation_full dsuicide_full daccount_balance_full ddebt_full dlife_expectancy dpopulation_growth



*HETEROGENEITY: 

twoway scatter GDP_growth_full country , msymbol(circle_hollow)

sort country year

by country: egen average_gdp= mean(GDP_growth_full)



*HETEROGENEITY ACROSS COUNTRIES:

//MORE ADVANCED SCATTER PLOT

twoway scatter GDP_growth_full country , msymbol(circle_hollow)


//MORE ADVANCED SCATTER PLOT

twoway scatter GDP_growth_full country , msymbol(circle_hollow) || connected average_gdp country , msymbol(diamond) ||, xlabel(1 "A" 2 "B" 3 "C" 4 "D" 6 "E" 6 "F")



* OUTLIERS


xi: reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

twoway (scatter uhat GDP_growth_full_hat )

rvfplot, yline(0)


//FOR EACH OF THE REGRESSORS SEPERATLY  

avplot trade_full 
avplot tax_rev_full 
avplot gross_fixed_cap_form_full 
avplot growth_labor 
avplot Govern__consumption_full 
avplot real_interest_rate_full 
avplot foreign_invest_inflow_full 
avplot foreign_invest_outflow_full 
avplot unemployment_full 
avplot inflation_full 
avplot suicide_full 
avplot account_balance_full 
avplot debt_full 
avplot life_expectancy 
avplot population_growth

//ALL TOGETHER

avplots



* PLOT OUTLIERS AND IDENTIFY THEM:

//GRAPHICALLY:

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

gen id = _n

lvr2plot, mlabel(id)

lvr2plot, mlabel( year )

lvr2plot, mlabel( country )


//RESIDUAL STATISTICS (COOK'S DISTANCE)

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

predict cooksd, cooksd

list cooksd if cooksd>4/90



*DROP OUTLIERS 

drop in 10

drop in 22

drop in 39

drop in 41

drop in 116

drop in 309



* COMPARISON OF ESTIMATES (OLS & WLS/GLS)

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

estimates store OLS


reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,vce(robust)

estimates store WLS


estimates table OLS WLS , star stats(N F r2)



//ESTIMATORS: OLS& FE & RE 

//OLS POOLED

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth

estimates store OLS_pooled 


//FOR HETEROSKEDASTICITY

reg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,vce(robust)

estimates store WLS



//FIXED EFFECT ESTIMATOR (FE)

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,fe

estimates store FE


//FOR HETEROSKEDASTICITY

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,fe vce(robust)

estimates store FE_robust


//RANDOM EFFECTS ESTIMATOR (RE)

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,re

estimates store RE

//FOR HETEROSKEDASTICITY 

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,re vce(robust)

estimates store RE_robust

//TABLE

estimates table OLS_pooled WLS FE FE_robust RE RE_robust, star stats (N F r2 r2_ο r2_b r2_w)



* COEFFICIENTS TESTING


//fixed effects estimation 

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth i.country 


//time fixed effects

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth i.year, fe

testparm i.year



* TEST: HAUSMAN SPECIFICATION, FE vs. RE estimator:

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,fe

estimates store FE_estimator



xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,re

estimates store RE_estimator


hausman FE_estimator RE_estimator    //accept H1=accept FE because p<a



*TESTING FE vs. OLS:

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,fe 

//accept H1=accept FE because p<a


* TESTING FE vs FE_robust

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,fe

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth,fe vce(robust)



*REGRESSION ANALYSIS 


*GENERATE NET IMPORTS VARIABLE

gen net_imports= fuel_imports-fuel_exports


*DUMMY CREATION 

//generate oil trader category dummy 

gen oil_trader_category = 0

replace oil_trader_category=1 if net_imports > 0 

tabulate oil_trader_category country

by country: gen freq = sum(oil_trader_category)

by country, sort: egen max = max(freq)

gen exp_countries = ccode if max<20

gen imp_countries = ccode if max>20


*RUN REGRESSIONS WITH IF STATEMENTS 

//Run regression for net importer countries 

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth if oil_trader_category == 1, fe vce(robust)


//Run regression for net exporter countries 

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full life_expectancy population_growth if oil_trader_category == 0, fe vce(robust)



*GENERATE SQUARED DEBT

gen debt_squared=debt_full^2



*MODEL WITH ROBUSTNESS ANALYSIS 

xtreg GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_full debt_squared life_expectancy population_growth oil_trader_category , fe vce(robust)



* PANEL INSTRUMENTAL VARIABLE ANALYSIS

ssc install ivreg2 

ssc install xtivreg2

ssc install ranktest


//correlation of variables

corr debt_full GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full life_expectancy population_growth


//IV Robust 

xtivreg2 GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full life_expectancy population_growth (debt_full=L.debt_full L2.debt_full L3.debt_full L4.debt_full L5.debt_full L6.debt_full L7.debt_full L8.debt_full L9.debt_full L10.debt_full unemployment_full tax_rev_full), endog(debt_full) fe robust

estimates store IV_robust 


//IV Robust with robustness analysis 

xtivreg2 GDP_growth_full trade_full tax_rev_full gross_fixed_cap_form_full growth_labor Govern__consumption_full real_interest_rate_full foreign_invest_inflow_full foreign_invest_outflow_full unemployment_full inflation_full suicide_full account_balance_full debt_squared life_expectancy population_growth oil_trader_category (debt_full=L.debt_full L2.debt_full L3.debt_full L4.debt_full L5.debt_full L6.debt_full L7.debt_full L8.debt_full L9.debt_full L10.debt_full unemployment_full tax_rev_full), endog(debt_full) fe robust

estimates store IV_robust_ranalysis


//Table

estimates table OLS_pooled WLS RE RE_robust FE FE_robust IV_robust IV_robust_ranalysis, star stats (N F r2 r2_ο r2_b r2_w)
