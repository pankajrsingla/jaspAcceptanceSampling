Accept/Reject Lots 
==========================
*Accept/Reject Lots* allows you to decide whether to accept or to reject a given lot, using pre-specified constraints and data collected from sample measurements.

## Input
-------
### Assignment Box
- **Measurement**: the observations/data collected for the sample.

## Options
- **Specify sample statistics directly (used if dataset is not available)**: sample statistics used for evaluating the lot in the absence of a dataset.
 - *Sample size (n)*: number of items in the sample.
 - *Sample mean*: mean of the sample values.
 - *Sample standard deviation*: Standard deviation of the sample values.
- **k**: the value used to compare the sample mean with the specification limits.
- **Specification limits**: the lower and the upper specification limits.
 - *Lower Specification Limit (LSL)*: the lower specification limit.
 - *Upper Specification Limit (USL)*: the upper specification limit.
- **Standard Deviation (Historical) known**: whether or not the historical standard deviation is known.
- **Quality constraints**: user-specified constraints associated with AQL and RQL.
 - *Acceptable Quality Level (AQL)*: the smallest proportion of non-conforming items that makes the lot definitely acceptable.
 - *Rejectable Quality Level (RQL / LTPD)*: the proportion of non-conforming items in the lot that is unacceptable to the consumer.

## Output 
-------
- **Decision table**: shows the sample statistics and the constraints, as well as the decision for the lot - whether to accept or reject it.

## References 
-------
- Kiermeier, A. (2008). Visualizing and Assessing Acceptance Sampling Plans: The R Package AcceptanceSampling. *Journal of Statistical Software*, 26(6), 1–20. https://doi.org/10.18637/jss.v026.i06.
- Lawson, J. (2021). An Introduction to Acceptance Sampling and SPC with R (1st ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781003100270.

## R Packages
-------
- jaspGraphs
- ggplot2
- AcceptanceSampling