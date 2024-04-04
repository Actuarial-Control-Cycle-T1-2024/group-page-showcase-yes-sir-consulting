Github Summary

Pricing Methodology

-   Significant disparities in historical mortality rates from the inforce dataset were found between smoking status and underwriting class

![A graph of smoking status Description automatically generated](media/187877d7a2babbe9c5d43bf51ab30243.png)![A graph of a graph with numbers and lines Description automatically generated with medium confidence](media/9143bfceb057800820629eb9666fe808.png)

-   A Cox Proportional Hazard model was used to capture relativities in modelling mortality rates

![A close-up of numbers Description automatically generated](media/caf82cb204624ac018bfffe97a419a8f.png)

-   ARIMA was used to model inflation and interest rates
-   Pricing was done according to the zero NPV principle

![](media/5785cadb7f8f7303edea4929d331fbf9.png)

Pricing Results

-   The proposed product is priced according to policy type, age group, smoking status and underwriting class. When offered at the same price, SuperLife would expect to earn on average Č243.94 more from the proposed product than current offerings.
-   Life expectancy is expected to improve; an individual aged 30 may also see an increase in life expectancy of up to 1.5 years.

![](media/d4b87ec36a8970a53344f8846e2f6077.png)

-   Average incurred loss per policy on the central estimate of assumptions is expected to be Č3.65 before the addition of any profit margins.
-   Sensitivity tests were conducted on prices assuming different levels of utilisation. All tested prices failed under extreme scenarios of low investment returns. The product may still perform well under high inflation if investment returns are healthy, due to the assumption that indexation is limited to 5%p.a. However, high inflation coupled with low returns sees the product incur the greatest total losses.
-   ![](media/a0e50963ddd1cab148cbe0e9d4676dba.png)

Assumptions

Rates of investment return and inflation: Modelled from historical one and ten year spot rates and historical inflation rates using ARIMA modelling, with 95% CI intervals capped at the min and max historical values to model high and low-rate scenarios

Payment Patterns and Indexation: Claims and claim expenses are paid once at the end of the year of death, renewal expense is incurred at the beginning of the year, and initial expense is incurred once at the beginning of the first policy year. The face value lump sum of the amount insured is indexed to inflation at a maximum of 5% per year. There are no refunds upon lapse. Level premiums are paid at the start of each year, ceasing upon death and lapse.

Expenses: Claims expense of Č5000 is assumed per policyholder. Intervention costs of Č155, derived from the upper-bound total costs for all incentives implemented. Initial expenses are Č500 upon policy start and yearly renewal expenses are assumed to be Č200 at the start of each subsequent year of survival.

Death and mortality rate: The mortality rates are modelled according to discrete ages, assumed to be constant between ages.

Lapse Rate: Policyholders lapse on term completion and do not extend. Historical Lapse rates by age group, policy type and gender were used.

Interventions on mortality: Interventions are assumed to have an additive reduction in mortality rates, with levels of utilisation corresponding to different reductions in mortality. No utilisation leads to no reduction in mortality. A low level of utilisation corresponds to a 2.0 % reduction in mortality, While a high level of utilisation corresponds to a 13.4% reduction in the mortality rate. The central estimate is the average of the two limits, at a 7.7% reduction (**Appendix G**).

Key Assumptions to Costs: While costs of interventions may vary, the assumption is to take the upper limit of combined costs as a conservative measure. Unforeseen increases in recurring costs, translate to lower profit margins and even possible incurred losses. As demonstrated in sensitivity testing (**Appendix H**), scenarios in which rates of return differ to those expected will cause profitability to vary greatly.

Utilisation: It is conservative to price assuming a central estimate of utilisation according to the mortality changes given in the research provided by SuperLife.
