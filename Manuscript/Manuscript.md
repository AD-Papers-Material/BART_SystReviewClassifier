Manuscript
================
Angelo D’Ambrosio
12/1/2021

## Results

### First search session

The first batch of searches, performed with the two manually curated
input queries, returned 9016 unique results, specifically 8867 unique
results from Pubmed, 467 from Web of Science, and 48 from IEEE.  
Of the records retrieved using the input queries, 1006 were manually
labeled as relevant or not for the topic of the systematic review. Of
these 51 (5.07%) were labeled as positive, and 955 (94.9%) as negative.

Subsequently, cycles of prediction/review tasks were performed, in which
manually reviewed records (R task) were used to train the ML
classification algorithm which is used then to reclassify all records (P
task).

| Task | Task type | Uncertain      | Positive       | Negative        | Reviewed |
|-----:|:----------|:---------------|:---------------|:----------------|:---------|
|    1 | R         |                | 51 (0.57%)     | 955 (10.6%)     | 1006     |
|    2 | P         | 61 (0.68%) +61 | 42 (0.47%) +4  | 8913 (98.9%)    |          |
|    3 | R         |                | 62 (0.69%) +11 | 987 (10.9%) +32 | 1049     |
|    4 | P         | 38 (0.42%) +17 | 34 (0.38%)     | 8944 (99.2%)    |          |
|    5 | R         |                | 61 (0.68%) -1  | 989 (11%) +2    | 1050     |
|    6 | P         | 46 (0.51%) +8  | 38 (0.42%)     | 8932 (99.1%)    |          |
|    7 | R         |                | 62 (0.69%) +1  | 991 (11%) +2    | 1053     |
|    8 | P         | 35 (0.39%) +2  | 37 (0.41%)     | 8944 (99.2%)    |          |
|    9 | R         |                | 62 (0.69%)     | 991 (11%)       | 1053     |
|   10 | P         | 47 (0.52%) +2  | 31 (0.34%)     | 8938 (99.1%)    |          |
|   11 | R         |                | 62 (0.69%)     | 992 (11%) +1    | 1054     |
|   12 | P         | 31 (0.34%)     | 38 (0.42%)     | 8947 (99.2%)    |          |
|   13 | P         | 38 (0.42%)     | 33 (0.37%)     | 8945 (99.2%)    |          |

Table 1. Prediction (P) / review (R) tasks’ results after the first
query was performed. The Uncertain column shows the number of new
labeled records not already reviewed in the precedent tasks and the
possible increment compared to the previous P task. The Positive column
has different meaning for the type of task: for the P tasks is the
number positevely labeled records followed by the number of new
positives not previously reviewed, if any; for the R task, the
cumulative number of positive and its increment, if any. The same
interpretation stands for the Negative column, albeit increments are not
shown for the R task, since the negative predictions are not reviewed.
All values are also shown as percentege over total of records available
when the task was performed.

### Second search session
