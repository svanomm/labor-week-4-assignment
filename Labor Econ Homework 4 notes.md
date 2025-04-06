Heckit model:
1. regress probit model for probability of being included
2. Regress y variable on controls plus the inverse Mills ratio

R code:
- library(sampleSelection)
	- heckit <- selection(, method = "2step")
	- summary(heckit)

### lecture
three ways to look at min wage
- DiD on state minimum wage
- Real federal minimum wage
file on canvas has state minimum wages over time
### **Assignment Description**
The process of internal migration, wherein individuals move from one region to another within a country, has become increasingly prevalent in recent years. The decision to migrate is often influenced by various factors, such as economic opportunities, lifestyle preferences, and personal circumstances. As a result, individuals who choose to relocate may possess unique characteristics that differentiate them from those who remain in their original location. These self-selection biases can significantly impact the estimation of wage equations for internal migrants.
**Research Questions:**
- What are the key determinants of internal migration within the U.S.?
- How does self-selection bias affect the estimation of wage equations for internal migrants?
- How can we effectively correct for self-selection when estimating the wage equation for internal immigrants?
**Methodology:**
To address the research questions, this paper will employ a rigorous econometric approach, incorporating a two-step strategy to correct for self-selection bias. The initial step involves identifying the factors influencing the decision to migrate internally. These factors will be estimated using a suitable model (e.g., probit or logit regression), allowing us to create a self-selection propensity score for each individual. In the second step, we will utilize this propensity score as a control variable in the wage equation estimation, thereby isolating the true impact of other variables on wage
**Data:**
From the American Community Survey, download data regarding key individual characteristics, migration history, labor market outcomes, and other relevant socio-economic factors.
### **Context/Purpose**
The minimum wage is a widely debated topic in economics, and its effects on the labor market, particularly on teenage employment, have been the subject of numerous studies. For this assignment, you are tasked with investigating the relationship between minimum wage changes and teenage employment levels using one of the following econometric methods: cross-section regression using state minimum wage, time series inflation-adjusted federal minimum wage, or a difference-in-differences estimator.
### **Requirements and Logistics**
1. Prepare a PowerPoint Presentation (8-10 slides) that covers the followings issues: 
2. Introduce the topic. 
3. Present the variable you retrieved from the ACS website and illustrate your econometric model. 
4. Include a table of the descriptive statistics of the data and of the regression output. 
5. Discuss your results and draw conclusions based on the concepts covered in the textbook.  
6. Reflect on the limitations of the data or methodology used and suggest possible areas for further research.


What are the key determinants of internal migration within the U.S.?
- Age: younger are more likely to move
- Education: educated are more likely to move
- Veteran status: veterans relocate more
- \# of children: more children makes it harder to move
- Cost of migration: proxy with distance
- Migration is family-centric, so need to know spouse's situation or household situation
- New employees are most likely to quit, because they're "testing the waters"
- Roy model factors: high-skill workers should move to areas with more income inequality, low-skill to with less income inequality
	- Expect high-skill to move to big cities
- https://www.nber.org/system/files/working_papers/w32123/w32123.pdf "migration is decreasing with origin wages and destination home prices, and is increasing with destination wages and origin home prices"
- https://papers.ssrn.com/sol3/Delivery.cfm?abstractid=4028260 "much of this relocation takes place during a critical period between a person’s labor market entry and mid-thirties"

https://papers.ssrn.com/sol3/Delivery.cfm?abstractid=4028260 papers have studied how Hurricane Katrina affected migration patterns, since it was an exogenous shock that caused mny ppl to leave


ACS Filters:
- Head of household
- 18-65 years old
- Regular household (not group quarters)
- Lived in a US state last year (Continental + AK/HI)

Variables of interest:
- Spouse characteristics: employed, veteran status

Borjas et al. (1992)
- Worker skill: # years education, aptitude test score, hourly wage, worker fixed effects
- equation: log(wage) = state FE, year FE, control variables (age, age squared, years of completed education, job tenure, union status, marital status, health status, metropolitan residence, industry, and occupation.)
- Relative high-skill workers are measured by deviation from state mean