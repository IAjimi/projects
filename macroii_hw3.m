%%%% MACRO PROBLEM SET 3
%%Go to bea.gov and download quarterly real GDP from 1969 to 2017.
%%(a) Take the natural log of the time series for real GDP (call itYt) 
%%and use the HPfilter (posted) to plot:  (i) lnYtand its trend (both on the same graph) 
%%and (ii)detrended lnYt.

%%%using Table 1.1.6. Real Gross Domestic Product, Chained Dollars
%%"Billions of chained (2012) dollars" Seasonally adjusted at annual rates

gdp = readtable('C:\Users\ia767\Downloads\download.csv') ; %reading data

smaller_gdp = str2double( table2array(gdp(3, 3:size(gdp,2))) ) ;
%transform table type to 'array' (technically speaking a cell), then converts to double type
%selects row 3 (GDP) & columns 3:end of df (all year columns)
%size(, 2) is number of columns

yt = log(smaller_gdp); %takes log of array

[T,C] = hpfilter(yt, 1600); % T = trend, C = cyclical components, 1600 for quarterly filter

x = linspace(1969, 2018, length(yt)); % x axis

plot(x, yt) %log gdp
hold on  %keeps plot to graph second line
plot(x, T) %gdp trend
hold off
title('Log Real GDP Over Time')
ylabel('Log Real GDP')
legend({'Log Yt','Trend'},'Location','southeast')
axis tight; %centers axis

plot(x, C) %cyclical component
hold on  
yline(0);
hold off
title('Detrended Log Real GDP')
ylabel('% Deviation from Trend')
axis tight;

%%% Question B
%%using Table 3.1. Government Current Receipts and Expenditures
%%[Billions of dollars] Seasonally adjusted at annual rates
gov = readtable('C:\Users\ia767\Downloads\download (1).csv');
smaller_gov = str2double( table2array( gov(22, 3:size(gov,2)) ) ) ;
gt = log(smaller_gov);

%very optional: subsetting using logical expressions
%cond = contains(table2array( gov(:, 2) ),"Current expenditures")
%smaller_gov = str2double( table2array( gov(cond, 3:size(gov,2)) ) ) ;
%%note:returns twice the same array

[GT,GC] = hpfilter(gt, 1600); % T = trend, C = cyclical components, 1600 for quarterly filter

plot(x, GC) %gov cycle
hold on  
plot(x, C) %gdp cycle
yline(0);
hold off
title('Real GDP and Government Spending Cycles')
ylabel('% Deviation from Trend')
legend({'Gt Trend','Yt Trend'},'Location','southeast')
axis tight; %centers axis

%%Correlations
corr(C, GC) %correlation detrended gdp, detrended gov
%-0.6175

GC_2 = [missing; GC(1:length(GC)-1)] ; %GC t-1
corr(C, GC_2,'rows','complete')
% -0.5910

GC_1 = [GC(2:length(GC)); missing] ; %GC t+1, adds missing value at the end for vector length
corr(C, GC_1,'rows','complete') %ignores missing values in rows
% -0.6331

%%Find the volatility of detrended lnGtin terms of the volatility of detrended lnYt
std(GC) / std(C)
%1.0004
