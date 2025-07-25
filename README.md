**Purpose**: This project aims to find important factors of well-being for 15-year-old students in cultural regions. 

**Data**: 2015 Program for International Student Assessment (PISA) [PISA 2015](https://www.oecd.org/pisa/data/2015database/).
The Student questionnaire data file was used. 

**Code**: 
1.	Data Management.R: This code deals with data management in nine cultural regions. 
2.	Linear regression_Mplus: fitting the linear regression in Mplus using 10 plausible values of sciences (**Type=imputation**)
3.	Imputation data for Mplus: saving 10 files for 10 plausible values to be read in Mplus. This code is for the code of Linear 
    regression_Mplus
4.	Quadratic regression_Mplus: Quadratic regression analysis for belonginess in a cultural region.
5.	Polynomial regression_Mplus: Polynomial regression analysis for belonginess in a cultural region.
6.	Randomforest.R: randomforest analysis for a cultural region (i.e., Anglo).
7.	Decision tree. Rmd: decision tree for a cultural region (i.e., Nordic)
8.  Nordic_19012021_Tuned.pdf: the documentation of the decision tree analysis in Nordic

**Publication**: 
Wu, Y. J., & Lee, J. (2022). The most salient global predictors of adolescents’ subjective Well-Being: Parental support, peer support, and anxiety. Child Indicators Research, 15(5), 1601-1629.
https://doi.org/10.1007/s12187-022-09937-1
