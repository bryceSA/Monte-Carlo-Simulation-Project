# 📊 Non-Parametric Robustness Simulation Study Verification

**Author:** Bryce Anderson  
**Date:** March 2024  

---

## 🧠 Introduction

The decommissioning of nuclear facilities is a critical environmental and safety undertaking.  
Sohn and Hong (2021) presented a robust simulation study using the **Sign test** and **Wilcoxon Rank Sum (WRS) test** to assess the cleanliness of nuclear decommissioning sites.  

Their work explores the *MARSSIM* (Multi-Agency Radiation Survey and Site Investigation Manual) approach — a structured methodology using **non-parametric statistical tests** for final status surveys that determine whether a site is clean.

This project aims to **reproduce and validate** their findings using Monte Carlo simulation, and to compare the results with **parametric alternatives** (specifically, t-tests).  
We analyze performance under varying statistical parameters to evaluate test power, Type I and II errors, and robustness.

---

## 🧾 Summary

In the MARSSIM approach, both the **Sign Test** and the **Wilcoxon Rank Sum Test** are applied because environmental contamination data often deviate from normality.  

- **Null Hypothesis (H₀):** The mean contamination exceeds the DCGL (Derived Concentration Guideline Level) — i.e., the site is **contaminated**.  
- **Alternative Hypothesis (H₁):** The mean contamination is below DCGL — i.e., the site is **clean**.

The objective is to **reject H₀**, confirming that the site meets the decommissioning criterion.


### Insights
- **Sign Test** remains robust across most cases.  
- **T-Test** highly sensitive to small samples and variance.  
- Non-parametric methods are more **cost-efficient** and **statistically stable** for nuclear site assessment.

---

## 💬 Discussion

This project deepened understanding of **non-parametric testing** in environmental data contexts.  

**Key Takeaways:**
- Non-parametric tests are preferable when data are skewed or sample sizes small.  
- Simulation studies reveal how subtle parameter changes affect test power.  
- R programming skills were enhanced — particularly through Monte Carlo simulation design and debugging.  

**Future Directions:**
- Simulate contamination using **log-normal** or **exponential** distributions.  
- Add **spatial variability** to mimic heterogeneous contamination.  
- Extend study to other robust tests for parametric cases.



## 📄 Full Report

You can read the complete paper here:  
➡️ [**Project Document (Full Paper)**](Project%20Document.docx)
