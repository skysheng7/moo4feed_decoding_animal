# 3. Methods

## 3.1 Animals, Housing, and Data Collection

Data were collected as part of a larger study described in detail by Sheng et al. [-@sheng2024redefining]. Briefly, 159 lactating Holstein dairy cows (12 primiparous, 147 multiparous; parity = 3.0 ± 1.1; mean ± SD) were enrolled over a 10-month period (July 2020 to May 2021) at The University of British Columbia (UBC) Dairy Education and Research Centre in Agassiz, BC, Canada. All procedures were approved by the UBC Animal Ethics Committee (protocol A19-0299). Cows were housed in a dynamic group of 48 in a pen equipped with 48 sand-bedded lying stalls, 30 electronic feed bins, and 5 electronic water bins (Insentec, Hokofarm, Emmeloord, the Netherlands). The electronic bins identified individual cows via radio frequency identification and recorded the cow identification, bin number, visit start and end times, and start and end weight of the feed or water in the bin for each visit [@chapinal2007validation]. Cows were milked twice daily and total mixed ration was delivered twice daily while cows were away for milking. On average, every 16 ± 3 d, 6 ± 2 cows were removed from the pen and replaced with new animals (i.e., regrouping event); individual cows remained in the trial for an average of 86 ± 54 d. Farm staff performed daily health checks, and a trained observer scored locomotion weekly using a 5-level scale [@flower2006effect] to identify lame cows. A portion of the data gathered in this research was previously published at Sheng et al. [-@sheng2024redefining].

## 3.2 Data Cleaning and Filtering

All data preparation and analysis were conducted using R (version 4.3.1; @R-base). To ensure that the data we analyze reflect the behavioural baseline patterns of cows when they are healthy and not in estrus, we applied stricter data cleaning and filtering criteria that extended beyond the data cleaning procedure described in our previous work [@sheng2024redefining]. Starting from 14,728 cow-days across 159 cows and 306 d, exclusions were applied at two levels. At the individual level, data were removed for specific cows on days affected by:

(1) health events, including illness episodes when cows were moved to the sick pen and when cows received veterinary treatment while remaining in the pen (e.g., mastitis, fever, injury), with a buffer period around each event (7 days before and 7 days after the recorded events) to account for potential behavioural changes during the prodromal and recovery periods (16 cows across 120 days, 197 cow-days).

(2) lameness, defined as two consecutive weekly locomotion scores >= 3 [@flower2006effect; @eriksson2020effects], with all days during the lame period removed until the cow was scored as sound again, plus a 7-d buffer before lameness onset (48 cows across 227 days, 679 cow-days).

(3) the first and last day each cow was present in the group, as data on these days would be incomplete (159 cows across 43 days, 285 cow-days).

(4) days when farm staff recorded interference with individual cows due to lose of ear tags, hoof trimming or other routine husbandry procedures, as these events may temporarily alter behaviour through handling stress or cows being away from the electronic feed and water bins for extended period of times (9 cows across 22 days, 24 cow-days).

(5) days when cows were not confirmed pregnant, as reproductive status (e.g., bred, fresh, open) may influence behaviour through oestrous cycling and associated hormonal fluctuations; only cow-days where the reproductive status was recorded as pregnant were retained (87 cows across 207 days, 2,695 cow-days removed).

At the herd level, data were removed for all cows on days affected by:

(1) estrus events, as any cow in heat potentially disturb the behaviour of all other cows in the group (55 estrus events, 2,381 cow-days).

(2) technical disruptions to the Insentec recording system, including equipment breakdowns, compressor failures, power outages, missing data, and severe weather events that compromised bin function (28 d, 1,281 cow-days).

(3) regrouping events when the herd dynamics is destabilized (16 events, 674 cow-days).

After filtering, 107 cows with sufficient data across 207 days (10-130 days of observation per cow), 6226 unique cow-days were retained.

## 3.3 Behavioral Variable Extraction

To process the raw feeding and drinking data collected by the Insentec system and extract meaningful behavioral variables, we developed an open-source R package called moo4feed <https://www.skysheng.io/moo4feed/> . This package provides a standardized workflow for handling electronic feed and water bin data and supports a broad range of analyses. The functionality used in this study spans five key areas: (1) data processing, configuration, and quality control; (2) agonistic replacement detection; (3) non-nutritive visit detection; (4) feed availability estimation; and (5) meal clustering and analysis. Note that moo4feed includes additional variables and functionalities beyond the scope of this paper, such as pairwise synchronicity analysis.

**Data Processing, Configuration and Quality Control.** The package includes a global configuration system that allows users to tailor data processing to their specific farm setup. Users can customize settings such as column name mappings, time zones, bin identifiers, and the physical layout of bins in the barn, making the tool adaptable to a wide range of data formats and facility designs. The package also includes an automated module for handling daylight saving time (DST) transitions across North American time zones. For any given year and time zone, this module identifies the exact dates and times of spring-forward and fall-back transitions. During fall-back transitions (when clocks move back one hour, causing chronological order of visits within the repeated hour to be ambiguous), the package removes all visits from the duplicated hour, since it is not possible to determine the true chronological order, and adjusts subsequent timestamps accordingly. During spring-forward transitions (when clocks move forward by one hour), visit timestamps are shifted forward by one hour for the remainder of the day. This automated handling ensures temporal consistency across all visit records without manual correction. Additional details on the reproducible workflow are provided in Section 3.5.

The first processing step converts raw visit data into a standardized format, with each record capturing the cow's identity, the bin number, the start and end times of the visit, and the start and end weight of the feed or water in the bin. Only days for which both feeder and drinker data files were available were retained for analysis. Feed and water visit data then go through automated quality control check, which includes multiple validation steps: checking whether the expected number of cows was present each day to warn users when cows went missing or lost their ear tags, detecting and correcting double detections (instances where the same cow was recorded at two different bins simultaneously, which indicates potential bin malfunction), flagging records with negative durations, intakes, or weight measurements, identifying abnormally large single-visit feed or water intakes, flagging cows that did not feed or drink after noon (which may indicate a lost ear tag or illness), and flagging bins with unusually low traffic or no visits (which may indicate equipment malfunction). Visits with implausibly long durations were flagged using customizable thresholds (set to >2,000 s for feed bins and >1,800 s for water bins in this study), as were visits with unrealistically large single-visit intake amounts (>8 kg for feed; >30 L for water) or excessively high intake rates (>0.008 kg/s for feed; >0.35 L/s for water). Days on which a cow recorded fewer than 10 visits were flagged as potentially incomplete, typically due to technical issues such as bin breakdowns, and cows with abnormally low or high daily feed intake (<35 or >75 kg/d) or daily water intake (<60 or >180 L/d) were also flagged. This validation procedure functions as a warning system rather than a data filter. All flagged records are retained in the dataset, and researchers receive a summary report to support informed, daily decision-making about data quality on farm.

Following this initial quality control, additional outlier detection was performed using a k-nearest neighbor (KNN) algorithm [@malini2017analysis] applied to scaled visit duration, intake, and intake rate. Unlike the traditional approach of removing data points beyond three standard deviations (SD) from the mean, which applies rigid rectangular cutoffs, the KNN method operates in multidimensional space, considering the relationships among all three variables at once. This enables the detection of visits that appear plausible on any single dimension but are implausible in combination. The KNN algorithm compares each data point to its nearest neighbors and flags those that are unusually distant from their local cluster. Users can adjust the number of neighbors (k = 50 in this study), the percentile threshold for flagging (0.04% of feed visits and 0.1% of water visits were flagged as outliers in this study), and the relative scaling of each variable to control how aggressively outliers are detected. The package also provides visualization tools that allow users to inspect flagged outliers in two-dimensional plots (e.g., intake versus duration, or intake rate versus intake) before deciding whether to remove them.

After cleaning, the feed and water datasets are combined, and daily summaries of basic feeding and drinking metrics are calculated. This step yields six conventional behavioral variables: (i) total daily feed intake (kg), (ii) total daily feeding duration (s), (iii) total number of feeding visits per day, (iv) total daily water intake (L), (v) total daily drinking duration (s), and (vi) total number of drinking visits per day.

**Agonistic Replacement Detection.** Replacements at the feed bins were identified using a validated algorithm following the method described by Huzzey et al. [-@huzzey2014automatic] and Foris et al. [-@foris2019automatic]. A replacement was recorded when one cow (the actor) entered a feed bin within 26 seconds of another cow (the reactor) leaving that same bin, serving as a proxy for the actor physically pushing the reactor away from the bin. Replacement events were discarded if the presumed actor was already feeding at a different bin at the time the reactor exited, as this would indicate that the reactor likely left voluntarily rather than being displaced [@foris2019automatic]. Daily agonistic behavior was summarized into two variables: (vii) total number of times the focal cow acted as the actor (displacing another cow from a bin) per day, and (viii) total number of times the focal cow acted as the reactor (being displaced from a bin) per day.

**Non-nutritive Visit Detection.** A visit was classified as non-nutritive if the cow's measured intake did not exceed the calibration error of the Insentec system (0.5 kg), meaning the cow visited a bin that contained feed but did not eat anything. This yielded one daily-level variable: (ix) total number of non-nutritive feed visits per cow per day.

**Feed Availability Estimation.** Feed delivery events were automatically identified when a bin's start weight increased by at least 5 kg (threshold is customizable) between consecutive visits, with deliveries to multiple bins grouped together if they occurred within one hour. This approach allows users to track both the frequency of feeding events throughout the day and the exact amount of feed added to each individual bin during every delivery. By combining this with visit-level bin weight data, it becomes possible to estimate how much feed remained in each bin at the precise moment a cow arrived, enabling us to identify cows that consistently are disadvantaged and end up accessing bins only after much of the fresh feed has already been consumed. For each feeding visit, the proportion of feed remaining in the bin at the time of the cow's arrival was estimated. This produced one variable: (x) median proportion of feed remaining in the bin at visit start (%), calculated as the daily median across all of a cow's visits to all the feed bins. The median was used in place of the mean as the measure of central tendency, given that the underlying distributions of these variables are often skewed and do not conform to normality assumptions. This variable indicates whether a cow consistently arrived at bins with ample feed or tended to access nearly depleted bins, helping flag individuals that may be socially disadvantaged and forced to eat leftover feed.

**Meal Clustering and Analysis.** Individual feed bin visits were grouped into meals using a density-based clustering algorithm: DBSCAN [@khan2014dbscan], which identifies clusters of temporally close visits based on two parameters: a maximum time gap between consecutive visits within a meal (the meal criterion), and a minimum number of visits required to form a meal. The meal criterion was determined in a data-driven manner rather than using an arbitrary fixed interval (e.g., 60 or 90 minutes), which fails to account for differences among farms, data collection methods, and individual animals' differences.

To derive the meal criterion, we first extracted all inter-visit intervals (i.e., the time gaps between consecutive feed bin visits from the same animal) pooled across all animals and days. Because these gap times are highly right-skewed (most gaps are short, but a few are very long), a log transformation was applied before fitting a two-component Gaussian Mixture Model (GMM) [@tolkamp1998satiety]. The two components naturally capture two distinct populations of gaps: short within-meal pauses and longer between-meal gaps. The meal criterion was then set to the point where the two fitted distributions intersect. The moo4feed package visualizes the gap distribution and the fitted mixture components so users can confirm the two populations are well separated for their own data. As a simpler alternative, the package also supports a percentile-based approach, where the meal criterion is set to a user-specified percentile of the gap distribution (e.g., the 90th percentile). The meal criterion can be derived at three levels of granularity: a single universal threshold pooled across all animals and days, an animal-specific threshold applied consistently across all days, or a threshold estimated separately for each animal on each day. In this study, we used a single pooled criterion derived from the GMM method. Meals were required to contain a minimum of two visits. The DBSCAN algorithm then assigned each visit to a meal cluster or flagged it as an isolated, unclustered visit. Timeline visualizations of the resulting meal clusters were generated for each animal on each day to allow visual inspection of the clustering results.

Once visits were grouped into meals, meal-level variables were computed by aggregating visit-level data within each meal and then summarizing across all meals within each cow-day. This step yielded six meal-level variables: (xi) total number of meals per day, (xii) median meal duration (s), (xiii) median number of visits per meal, (xiv) median feed intake per meal (kg), (xv) median percentage of time spent feeding per meal, calculated as the proportion of the meal's total duration during which the cow was actively occupying a feed bin (total visit time ÷ meal duration × 100%), and (xvi) median number of non-nutritive visits per meal.

In total, 16 behavioral variables were included in the statistical analyses described in Section 3.4: six basic daily feeding and drinking variables (i–vi), two daily agonistic variables (vii–viii), one non-nutritive visit variable (ix), one feed availability variable (x), and six meal-level variables (xi–xvi).

## 3.4 Statistical Analysis

We used Bayesian mixed-effects models to quantify repeatability and predictability. All models were fitted using the brms R package [@burkner2017brms], which implements Bayesian regression models via the Stan programming language.

**Variable distribution assessment.** Before fitting any models, we examined the distribution of each behavioral variable by plotting histograms on both the raw and log-transformed scale. For each variable, we computed skewness, the proportion of zero values, and conducted Shapiro-Wilk normality test. This informed the likelihood family selected for each Bayesian model: variables with approximately symmetric distributions were modeled using a Gaussian likelihood (feed intake, feeding duration, water intake, median percentage of time spent feeding per meal, median proportion of feed remaining when cow starts feeding), while strongly right-skewed variables were modeled using a lognormal likelihood (number of feeding visits, drinking duration, number of drinking visits, total meals, median meal duration, median visits per meal, median feed intake per meal, number of non-nutritive visits, median non-nutritive visits per meal, total actor events, and total reactor events). Three variables modeled with a lognormal likelihood (median non-nutritive visits per meal, total actor events, and total reactor events) were shifted by adding a constant of 1 prior to model fitting, given that the lognormal distribution requires strictly positive values and each of these variables contained zero values. Posterior estimates for these variables were shifted back to the original scale for interpretation.

**Repeatability.** For each of the 16 behavioral variables, a Bayesian mixed model was fitted:

$$
\text{response} \sim \text{DIM} + \text{parity} + \text{THI}_{\text{mean}} + \text{poly}(\text{month},\, 2) + (1 \mid \text{cow})
$$

where DIM (days in milk), parity, and mean daily THI were fixed effects controlling for changes in physiological state and thermal environment, a second-degree polynomial of calendar month captured seasonal trends, and cow identity was a random intercept. Default brms priors were used. MCMC sampling used 4 chains with 1,000 warmup iterations; total iterations ranged from 6,000 to 17,000 per variable to ensure convergence.

Among-individual variation was quantified using two metrics derived from posterior draws: repeatability **(R)** and the coefficient of individual variation **(CVi)** [@hertel2020]. R is the proportion of total variance attributable to between-cow differences:

$$
R = \frac{\text{var.cow}}{\text{var.cow} + \text{var.res}}
$$

where $\text{var.cow}$ is the posterior variance of the cow-level random intercept and $\text{var.res}$ is the residual variance. R is thus confounded by the within-individual variance. A high value of R could mean cows differ a lot from each other, or simply mean that each cow is very consistent within herself. To resolve this ambiguity, we also computed CVi, which calculates among-individual spread relative to the population mean, independently of within-individual consistency:

$$
\text{CVi} = \frac{\sqrt{\text{var.cow}}}{\bar{y}}
$$

where $\bar{y}$ is the mean of the response variable. For lognormal models, CVi was instead computed as $\sqrt{\exp(\text{var.cow}) - 1}$ to place the estimate on the original data scale. CVi is interpretable across behaviors with different units and facilitates comparison across studies. Model convergence was assessed via R-hat (< 1.01), Bulk- and Tail-Effective Sample Size (ESS), trace plots, and posterior predictive checks. Individual-level posterior distributions were visualized using ridge plots ordered by each cow's posterior mean.

**Predictability.** Within-individual variance was quantified using a double hierarchical generalized linear model (DHGLM) implemented in brms. In addition to modeling the mean response, brms allows a variance structure to also be imposed on the residual variance (i.e., within-individual variance). Each DHGLM extended the repeatability model by adding a cow-level random effect to the residual variance, partitioning it per individual so that cows with high residual variance are identified as more unpredictable than cows with low residual variance:

$$
\text{sigma} \sim (1 \mid \text{cow})
$$

Population-level predictability was summarized using two metrics derived from the posterior SD of the cow-level random effect in the sigma sub-model ($\text{sd.sigma.cow}$). The residual intra-individual variability **(rIIV)** captures how much cows differ from one another in their within-individual consistency [@hertel2020]:

$$
\text{rIIV} = \exp(\text{sd.sigma.cow})^2
$$

We also computed the coefficient of variation of predictability **(CVP)**, which can be used to compare predictability across different behavioural traits and studies, independent of the scale of the trait:

$$
\text{CVP} = \sqrt{\exp(\text{sd.sigma.cow}^2) - 1}
$$

Both rIIV and CVP are monotonic transformations of the same parameter ($\text{sd.sigma.cow}$). We report both because rIIV has a direct biological interpretation as a fold-ratio in residual SD across individuals, whereas CVP provides a standardized, scale-free quantity for comparison across traits and studies.

A high CVP value means that cows differ considerably from each other in how consistent their day-to-day behavior is: for example, some cows may eat nearly the same amount of feed every day, while others fluctuate substantially. A low CVP value means that cows are similar to each other in their predictability: whether that means all cows are highly consistent, or all cows are highly variable, there is little difference between individuals in their within-individual consistency. MCMC sampling settings were identical to those used for the repeatability models.

Behavioral variables that showed both high CVi and low CVP are the best candidates for individual profiling, as these are behaviors that differ meaningfully across individuals and remain stable within individuals over time.

**Behavioral variable clustering.** To organize the 16 behavioral variables by how informative they are for individual profiling, we applied k-means clustering (k = 3, 50 random starts) to the posterior mean CVi and R values for each variable, after scaling both dimensions. The number of clusters (k = 3) was determined by researcher judgment based on visual inspection of the CVi and R distributions. All variables have low predictaibility values, thus measures of predictability were not used in behavioral variable clustering. The three clusters were labeled by descending R and CVi: Cluster 1 (high R, high CVi: traits in which cows differ substantially in their individual average behavioral expression relative to the herd mean, and those individual differences are consistent over time); Cluster 2 (high R, low CVi: traits in which individual differences are consistent over time, but the variation among cows is small relative to the herd mean); and Cluster 3 (low R, low CVi: traits in which cows show little variation relative to the herd mean, and what little variation exists is not consistent over time). We used only the variables in Cluster 1 for the downstream cow clustering, as they are the most informative for individual cow profiling.

**Individual-level behavioral profiles.** For each cow and each behavioral variable in Cluster 1, we extracted the individual intercept from the posterior distribution: the population intercept plus each cow's random effect (back-transformed to the original scale for lognormal models). This represents a cow's average behavioral type (i.e., a cow's typical expression of their behavior).

**PCA and cow clustering using k-means.** To synthesize the multi-dimensional behavioral profiles into a small number of interpretable dimensions and identify distinct cow types, we applied principal component analysis (PCA) with varimax rotation [@budaev2010using] to the matrix of individual intercepts for the Cluster 1 variables (one row per cow, one column per variable). The number of components retained was chosen to explain at least 80% of the cumulative variance.

Following PCA, cows were grouped into behavioral types by applying k-means clustering to the retained rotated component scores. The optimal number of clusters $k$ was determined using the silhouette method, evaluated across 100 independent runs with different random seeds, each using 100 random centroid initializations for stability. For each run, the value of $k$ yielding the highest mean silhouette score was selected, and the $k$ chosen most frequently across all seeds was used for the final clustering.

The silhouette score quantifies how well each data point fits its assigned cluster relative to neighboring clusters. For each cow $i$ belonging to cluster $C_i$, we first compute $a(i)$, the mean distance from cow $i$ to all other cows within the same cluster:

$$
a(i) = \frac{1}{|C_i| - 1} \sum_{j \in C_i,\, j \neq i} d(i, j)
$$

where $|C_i|$ is the number of cows in cluster $C_i$ and $d(i, j)$ is the Euclidean distance between cows $i$ and $j$. A smaller $a(i)$ indicates that cow $i$ is tightly grouped with its cluster members.

We then compute $b(i)$, the mean distance from cow $i$ to all cows in the nearest neighboring cluster (i.e., the cluster $C \neq C_i$ that minimizes the expression below):

$$
b(i) = \min_{C \neq C_i} \frac{1}{|C|} \sum_{h \in C} d(i, h)
$$

A larger $b(i)$ indicates that cow $i$ is well-separated from neighboring clusters. The silhouette score for cow $i$ is then defined as:

$$
s(i) = \begin{cases}
  1 - \dfrac{a(i)}{b(i)}, & \text{if } a(i) < b(i) \\[6pt]
  0, & \text{if } a(i) = b(i) \\[6pt]
  \dfrac{b(i)}{a(i)} - 1, & \text{if } a(i) > b(i)
\end{cases}
$$

Values of $s(i)$ range from $-1$ to $1$, where values close to $1$ indicate that the cow is well-matched to its own cluster and clearly separated from others, values near $0$ indicate borderline assignment, and negative values suggest possible misclassification.

The mean silhouette score $\bar{s}$ across all cows serves as an overall measure of cluster quality, reflecting both within-cluster cohesion and between-cluster separation. The resulting clusters were visualized using component score biplots and an interactive 3D scatter plot for exploration of cow clusters across three principal components.

## 3.5 Reproducible Data Science Workflow and Software

To support full transparency and reproducibility, all data processing, variable extraction, and analytical steps were implemented in the moo4feed R package <https://www.skysheng.io/moo4feed/>, which is publicly available with detailed tutorials. The functions implemented in this R package have been thoroughly tested with 96% code coverage (code coverage report: <https://app.codecov.io/gh/skysheng7/moo4feed>). This R package enables future researchers to replicate our variable extraction pipeline on their own datasets and to extend the approach to additional behavioral measures. The source code and example data included in the R package is hosted on GitHub: <https://github.com/skysheng7/moo4feed.git>. We used the newly developed R package to extract the behavioral variables described in Section 3.3 and conduct all statistical analyses described in Section 3.4, data and code for all the data analysis are available at: <https://github.com/skysheng7/competition_dominance_analysis.git>. We have also used Borealis to archive the R package at: <https://doi.org/10.5683/SP3/LF4DRO> and data analysis code at: <https://doi.org/10.5683/SP3/K6J350>.
