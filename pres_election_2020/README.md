**Predicting the 2020 Presidential Election**

The most time-consuming part of this project was data collection and cleaning. Data sources include MIT's Election Lab; FiveThirtyEight; the Bureau of Economic Analysis (BEA); and the U.S. Census Bureau.

Variables include the vote share of the Democratic party candidate of past Presidential, House, and Senate elections; per capita personal income per state and year; population per state and year; national GDP, exports, and personal consumption expenditures in the 3rd quarter of the election year; the percentage of the state population having graduated High School; the percentage of the state population having a Bachelor's degree or higher; the percentage of Black and White people per state; the average polling of the Democratic party candidate in the polls in the 15 days leading up to the election; and the % of the population having voted in the past Presidential election. 

Variables were selected using a custom backward selection function, minimizing the MSE of the model on *out-of-sample* elections. To avoid information leakage, the model is fit on elections between 1980 and 2000, then iteratively tested on the elections between 2004 and 2016 (four elections: 2004, 2008, 2012, 2016), including all past elections and excluding all future elections. Thus, the model is first fit on 1980-2000 and tested on 2004, then fit on 1980-2004 and tested on 2008, and so on.

A linear regression model using the best predictors is then fit on 1980-2016 and used to predict the vote share of the 2020 Democratic party candidate, Joe Biden, for all states. A logit model is then fit to convert the predicted vote share into a likelihood of the Democrat candidate winning a state. Those probabilities are subsequently used in a simulation to estimate the candidate's chance of winning the election and form a confidence interval of the number of electoral votes the candidate is expected to win.

*Potential Improvements*
* Improving and extending the variable selection function
* Refining Electoral College vote simulation

*Competing Forecasts*
* [FiveThirtyEight's Forecast](https://projects.fivethirtyeight.com/2020-election-forecast/) & [Methodology](https://fivethirtyeight.com/features/how-fivethirtyeights-2020-presidential-forecast-works-and-whats-different-because-of-covid-19/)
* [The Economist's Forecast](https://projects.economist.com/us-2020-forecast/president)
