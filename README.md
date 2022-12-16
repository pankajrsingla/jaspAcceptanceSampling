# The Acceptance Sampling Module

Create and analyze sampling plans to decide whether to accept or to reject lots.

Currently, the module supports:
- Creation of single stage attribute sampling plans that satisfy risk constraints
- Analysis of single stage as well as multiple stage attribute sampling plans
- Creation of single stage variable sampling plans that satisfy risk constraints
- Analysis of single stage variable sampling plans
- Comparing data from lots against single stage variable sampling plans and deciding whether to accept or to reject the lot(s)

## Module Structure

The analyses in the Acceptance Sampling module in JASP are structured in the following way:

```
--- Acceptance Sampling
    -- Attribute Sampling
       - Create Attribute Plan
       - Analyze Attribute Plan
    -- Variable Sampling
       - Create Variable Plan
       - Analyze Variable Plan
       - Accept/Reject Lots
```