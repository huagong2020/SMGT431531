# Teaching guide

These units continue the R, prediction, model evaluation, and neural-network material in Lectures 1–3. Students should understand linear and logistic regression, matrix multiplication, conditional probability, and training/test separation. Introduce Bayesian probability and conjugacy before assigning posterior computation.

The full decks support roughly two to three meetings per unit, depending on mathematical background. The core routes below support a 75-minute concept class followed by a 60–90-minute lab. Other sections can serve as follow-up lectures or optional reading. Slide headings and footers identify the matching tutorials.

## Lecture 4: Player performance evaluation

**Core class:** 4.1, the scalar model and shrinkage derivation in 4.2, context adjustment in 4.3, and conjugacy plus the empirical/full Bayes distinction in 4.4. Allow 15 minutes for sample-size differences, 25 for partial pooling, 20 for Bayesian interpretation, and 15 for a worked comparison.

**Lab:** 4.7. Run the full Gibbs model, inspect diagnostics, compare two passers, and produce future-performance intervals. The inverse-gamma parameterization is explicit because rate, scale, variance, and standard deviation are common sources of confusion.

**Extensions:** varying slopes and crossed effects in 4.3, grid/Laplace/MH/Gibbs/HMC in 4.5, and RAPM in 4.6. Students ready for matrix algebra can derive the weighted ridge posterior.

**Discussion:** What is the estimand? What is being pooled? Which parameters are fixed at estimates? Does the interval describe a mean or a future outcome? Which unmeasured context may be attributed to the player?

## Lecture 5: Team performance evaluation

**Core class:** 5.1, the design matrix and ridge connection in 5.2, the attack/defense structure in 5.4, and forecast timing in 5.6. Allow 15 minutes for schedule imbalance, 20 for team contrasts and pooling, 20 for the Poisson hierarchy, and 20 for prediction and checking.

**Lab:** 5.7. Sample the full hierarchy for team margins, compare strengths using joint draws, and forecast games after the information cutoff. Distinguish the fixed Week-10 forecast from weekly updating.

**Extensions:** Bradley–Terry and ordered outcomes in 5.3, Elo and Kalman filtering in 5.5, and posterior season simulations in 5.4. Use the market discussion as an evaluation benchmark and an example of the difference between estimated probabilities and reliable decision evidence.

**Discussion:** Why does adding a constant to every rating leave predictions unchanged? Why do opponent ratings have posterior covariance? Do fitted hyperparameters use future games? Is a curve filtered at retrospective parameter estimates or a real-time forecast?

## Lecture 6: Clustering methods for sport

**Core class:** feature and distance choices in 6.1, k-means/PAM in 6.2, linkage in 6.3, GMM responsibilities and BIC in 6.4, and a selection of 6.5. Reserve at least 15 minutes for validation in 6.6.

**Lab:** 6.7. Construct shot compositions, fit fuzzy c-means, compare membership concepts, and propagate count uncertainty through clustering. This introduces advanced methods through one sports question.

**Extensions:** DBSCAN/HDBSCAN for shot regions, spectral clustering for connectivity, NMF for additive shot patterns, and Gower/PAM for mixed scouting features. The from-scratch algorithms are teaching implementations with package examples for comparison.

**Discussion:** Are we grouping players, shots, or games? Do the features measure style or ability? What does a membership score mean? What changes in the stability experiment? Does the distance match the feature geometry?

## Guided-lab grading rubric

| Criterion | Points |
|---|---:|
| Sports question, observational unit, and target | 15 |
| Mathematical specification and correct notation | 25 |
| Reproducible R workflow and data separation | 20 |
| Diagnostics, validation, and uncertainty | 25 |
| Interpretation and limitations | 15 |

Tutorials include practice questions and collapsed discussion answers. For assignments, ask students to choose a different player comparison, matchup, feature set, or clustering resolution and defend it. Require a written interpretation so running code alone is insufficient.

## Notation conventions

- Mathematical `N(mean, variance)` uses variance as its second argument. R's `rnorm()` and `dnorm()` use `sd`.
- Subscripts index observations, groups, or time. Superscripts in parentheses index posterior draws or algorithm iterations, as defined locally.
- Bold symbols denote vectors or matrices. A superscript T denotes transpose.
- Each model defines its observation and group indices. Clustering labels are categorical, not ordered performance scores.
- Credible intervals describe uncertainty conditional on the data, model, and priors. Predictive intervals also include future variation. Bootstrap intervals describe a stated repeated-sampling procedure.
- Empirical Bayes conditions on estimated population parameters. Full Bayes integrates their posterior uncertainty. Conditional Gaussian means and modes coincide; logistic mixed-model modes generally differ from posterior means.
