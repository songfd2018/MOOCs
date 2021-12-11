# Validity check for peer assessment schemes in MOOCs

We offer a linear-time-complexity algorithm to quickly determine the validity of a given peer assessment scheme. Compared with an alternative approach, the speed of BFS-MOOCs accelerates over 1,000 times. At the same time, the disconnected submission communities can be identified by BFS-MOOCs. The instructor can grade a small number of submissions in each disconnected community to turn the shared grading graph into a connected graph. Thus, BFS-MOOCs provides a strategy for the instructor to remedy an invalid observed assessment grid. The following steps will reproduce the results in our manuscript.

## Efficiency in validity check

We propose an efficient algorithm, BFS-MOOCs, to check the validity of a peer assessment scheme built upon the breadth-first search (BFS) algorithm. The `01Identifiability_Check` folder contains code that generates Fig 4 in the manuscript. To reproduce all the results, please run `Comparision_in_idenfiability_check.R` to:

- conduct the BFS-MOOCS and determined-based algorithm under the circulating designs with four levels of class size N = 1,000, 3,000, 10,000 and 30,000 and five levels of the number of assigned submissions m = 3, 4, 5, 6, 7 for each grader;
- store the probability and time consumption under overall designs in `id_check_consumption.RData`
- draw the line chart of the time consumption versus the class size N under different m.

## Impact of missing grading and sample size calculation

Even if an instructor assigns the class a valid assessment grid W, the resulting assessment grid Y realized by the students can still be invalid because students may not complete their peer assessment tasks. We propose a dynamic programming algorithm to calculate the probability that the resulting observed assessment grid Y is valid given the assigned assessment grid W and grader-specific grading probabilities.

First, compile the C++ source code in the folder `02Missing_Grading/src` by running

```
cd 02Missing_Grading/src
R CMD SHLIB cal_prob.cpp
```

Second, run `Calculate_connectivity_prob.R` to:

- calculate the probability of realizing a valid Y recursively

`Calculate_connectivity_prob.R` requires `Rscript` command to input arguments, for example:

```
cd ..
Rscript Calculate_connectivity_prob.R -N 1000 -m 5 -t 0.8
```

where 

- the argument -N denotes the class size, 
- the argument -m denotes the number of assigned submissions to each grader, 
- and the argument -t represents the grading probability. 

The function returns the probability of realizing a valid Y and time consumption. The function also generates an `RData` file to store the probability and time consumption in the `./result` directory.

In our manuscript, we vary N among 1,000, 3,000, 10,000, vary m among 3, 4, 5, 6, 7 and vary lambda among 0.6, 0.7, 0.8 and 0.9. 

After generating `RData` files under all different settings of (N, m, lambda), run `Plot_probability_trend.R` to:

- collect the valid probabilities and time consumptions under overall settings, which is stored in `./result/prob_trend_collection.RData`
- draw the line chart of the probability of realizing a valid Y versus m under different N and lambda.

## Ordering of students with different attitudes in peer assessment

In reality, different students may have different attitudes towards the peer assessment tasks. It is of interest to consider how the ordering of students will affect the validity of the observed assessment grid. Thus, we propose simulated circumstances to study the impact of the student distribution in the peer assessment. Suppose that there are a total of 1,000 students in a MOOC. Half of them are active with 90% grading probabilities, while the others are reluctant with 70% grading probabilities. Under the circulating peer assessment scheme, we consider the impact of different grader orders on the the corresponding connectivity probabilities. Let `2l` students as a unit, where the first `l` students are active and the last `l` ones are reluctant. We vary `l` among 1, 2, 4, 5, 10, 20, 50, 100, 250 and 500.

Again, compile the C++ source code in the folder `03Arrangement_Grader/src` by running

```
cd 03Arrangement_Grader/src
R CMD SHLIB cal_prob.cpp
```

Then, run `Arrangement_comparison.R` to:

- generate the grading probabilities vector `prob_N1000_consecl.txt` for each l as the input of probability calculation;
- calculate the connectivity probability by running `Calculate_connectivity_prob.R` as

```
Rscript ../02Missing_Grading/Calculate_connectivity_prob.R -N 1000 -m m -n prob_N1000_consecl
```

where the argument -n denotes the file name of the grading probability vector of all students. Here, we vary m from 4 to 6 and vary l among {1, 2, 4, 5, 10, 20, 50, 100, 250, 500};

- draw the line chart of the probability of realizing a valid Y under different circumstances.

## Finding all subgraphs in the observed assessment grid

When a realized peer assessment scheme is unfortunately invalid due to missing grading, instructors can recursively run the BFS-MOOCS algorithm to identify all subgraphs of the shared grading graph. Then, if the instructor grades at least one submission in each subgraph to make the shared grading graph connected, then the assessment grid will become valid.

Here, we visualize the identified subgraphs in a simulated assessment grid by running `Finding_subgraphs.R` in the folder `04Subgraph_Identification`. 

