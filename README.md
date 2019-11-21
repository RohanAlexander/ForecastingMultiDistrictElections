# ForecastingMultiDistrictElections

## Overview of paper
The overall outcome of an election in a parliamentary system turns on the number of electoral divisions won by each party. Despite this, traditional polling tends to focus on broad national-level estimates of support for major parties. In this paper we develop a framework to characterise a useful forecasting model of parliamentary elections. We then implement an approach to forecast the number of electoral divisions won by each of the larger political parties in Australia at the 2019 Federal Election. Our approach first uses survey data and multi-level regression with post-stratification (MRP) to estimate electoral-division-level first-preference shares. To account for Australia's use of preferential voting, we then use supervised machine learning to estimate electoral-division-level two-party-preferred shares. We find that our approach performs well out-of-sample, and it has additional advantages over traditional polling including improved interpretability, transparency, and better communication of statistical uncertainty. Our framework allows the consistent evaluation of forecasting models of elections in a parliamentary system. Finally, our paper also improves our understanding of the circumstances in which MRP is appropriate. 

## Note on file structure
'Inputs' are everything that we can't/shouldn't edit - other papers, raw data, etc. 'Outputs' is where anything that we created/modified lives. So there's a data folder in there for data that we modify, and the paper is in there.

## To do
- (Patrick) Get education and unemployment.
- (Rohan) Add the Pat^2 idea of getting an estimate of the number of people that it would take to make the results equivalent.

