# Comparing New Jersey's traffic data to the nation

I was just wondering how safe New Jersey's roads were compared to the rest of the nation, and performed a study to do this.

In order to see how New Jersey's road safety compares with the rest of the nation, a dataset from with the traffic data from the nation was pulled from here: <https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents> (cited below). The variables of interest were Severity and State.

The dataset is "big data" which R is not so good with, so was placed on a MariaDB server and the RMariaDB package was used to query the database and pull the fields of interest.

The dplyr function sample_n was used to pull a 1000 unit random subsample from each state in order to produce the dataset for this study. Unforunately, the raw dataset is far too large to use.

Since we are attempting to determine a relationship between an ordinal variable (Severity defined as 1,2,3 and 4) and a nominal variable (State), we should first determine if the nominal variable has a significant effect on the ordinal variable, and then use a post-hoc test to see the significant contrasts. The most appropriate test for this is the Kruskal-Wallis test.

```{r}
kruskal.test(Severity~State,data=trafDataSample)

```

The above model showed that State has a significant effect on the severity of accidents. We then had to determine effect size to see if this has a real world effect. The appropriate test would be the epsilon-squared test

```{r}
library(rcompanion)
epsilonSquared(trafDataSample$Severity, trafDataSample$State)
```

This showed a moderate effect, so we can confidently move forward with a post-hoc test. It was difficult to find a post-hoc test that was ok with a ordinal vs nominal test, but eventually the kruskalmc test was used.

```{r}
library(pgirmess)
datMC <- kruskalmc(Severity~State,data=trafDataSample)
```

The important results of this test, namely the significance of contrasts, we then saved and used for later models.

I decided to then compare two metrics, the average accident rating and the percent chance an accident will be severe. The average accident rating used a poisson regression and used the ordinal variables as numbers. The regression was likely not necassary, and I could have probably just taken the mean accident rating of each State.

The percent chance of severity was determined by taking turning severity into a binomial distribution. Severety levels 1 and 2 were counted as not severe and levels 3 and 4 were considered severe. A binomial generalized linear model was then used to determine significance and then the emmeans package was used to determine the odds ratio.

The final table I made has comparisons to New Jersey. What was interesting to me is that many regions in the mid-west have higher accident ratings and higher probabilities of a severe accident then NJ, despite NJ being the most densely populated state.

The first model attempted to use the ordinal variables representing severity (1,2,3,4, with 4 being the most severe and 1 being the least) as integer values, and a poisson distribution was used. Unfortunately, the qqplots and other assumptions failed.

-   Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, and Rajiv Ramnath. “[A Countrywide Traffic Accident Dataset](https://arxiv.org/abs/1906.05409).”, 2019.

-   Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, Radu Teodorescu, and Rajiv Ramnath. ["Accident Risk Prediction based on Heterogeneous Sparse Data: New Dataset and Insights."](https://arxiv.org/abs/1909.09638) In proceedings of the 27th ACM SIGSPATIAL International Conference on Advances in Geographic Information Systems, ACM, 2019.
