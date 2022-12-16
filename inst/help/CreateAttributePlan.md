Create Attribute Plan 
==========================
*Create Attribute Plan* allows you to generate a 2-class single stage attribute sampling plan that satisfies specific quality constraints. This type of sampling involves classifying each inspected item as either acceptable or not acceptable.

## Input
-------
## Options
- **Lot size (N)**: total number of items in the lot. Note that if hypergeometric distribution is specified, the proportion of non-conforming items * N gives the number of non-conforming items, which must all be non-negative integers.
- **Quality constraints**: user-specified constraints associated with AQL and RQL.
 - *Acceptable Quality Level (AQL)*: the smallest proportion of non-conforming items that makes the lot definitely acceptable.
 - *Rejectable Quality Level (RQL / LTPD)*: the proportion of non-conforming items in the lot that is unacceptable to the consumer.
 - *Producer's Risk (α)*: Risk associated with the rejection of a lot that has acceptable quality. Equals the probability of rejecting a lot at AQL.
 - *Consumer's Risk (β)*: The probability of accepting an RQL quality lot.
- **Proportion non-conforming items**: the range of quality levels at which the plan will be analyzed.
 - *From*: lower bound of the quality level range.
 - *To*: upper bound of the quality level range.
 - *Step size*: difference between consecutive quality levels.
- **Distribution**: the distribution assumed to be followed by the number of non-conforming items in the inspected sample(s).
 - *Binomial*: (default) Should be used when the lots come from a continuously ongoing production process, and the lot size (N) is much larger (>= 10 times) than the sample size (n). The probability of drawing non-conforming items from the lot is assumed to be constant for each sample.
 - *Hypergeometric*: Should be used when isolated lots of finite size are inspected. The exact lot size (N) needs to be specified.
 - *Poisson*: Should be used when the lots come from a continuous production process, and the rate of defects is measured for each sampled item. Recommended for cases where sample size (n) is large, and the proportion of non-conforming items (PD) is small.
- **Output options**: the tables/plots to be generated for the generated attribute plan.
 - *Summary table*: show the acceptance probabilities at each of the points in the quality level range specified through PD.
 - *OC curve*: display the operating characteristics (OC) curve for the sampling plan at the quality levels specified through PD.
 - *AOQ curve (plan with rectification)*: display the long term outgoing quality level of the sampling plan (with rectification) when rejected lots are 100% inspected, and all non-conforming items are replaced with conforming items.
 - *ATI curve (plan with rectification)*: display the long term total number of items inspected when rejected lots are 100% inspected, and all non-conforming items are replaced with conforming items. 

## Output 
-------
- **Basic sampling tables**: shows the tables of the generated attribute plan, and of the acceptance and rejection probabilities at AQL and RQL.
- **Summary table**: shows the acceptance probabilities at every point in the quality level range.
- **OC curve**: plots the operating characteristics of the plan.
- **AOQ curve**: plots the outgoing quality levels against the incoming quality levels, for a plan with rectification.
- **ATI curve**: plots the average number of items inspected against the incoming quality levels, for a plan with rectification.

## References 
-------
- Kiermeier, A. (2008). Visualizing and Assessing Acceptance Sampling Plans: The R Package AcceptanceSampling. *Journal of Statistical Software*, 26(6), 1–20. https://doi.org/10.18637/jss.v026.i06.

## R Packages
-------
- jaspGraphs
- ggplot2
- AcceptanceSampling