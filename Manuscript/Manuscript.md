Manuscript
================
Angelo D’Ambrosio
12/1/2021

## Results

### First search session

The first session of searches, performed using the two manually curated
input queries, returned 9016 unique results, specifically 8867 unique
results for Pubmed, 467 for Web of Science, and 48 for IEEE.  
Of these records, 1006 were manually labeled as relevant or not for the
topic of the systematic review. Of these 51 (5.07%) were labeled as
positive, and 955 (94.9%) as negative.

Subsequently, cycles of prediction/review tasks were performed, in which
manually reviewed records (R tasks) were used to train the ML
classification algorithm which is then used to reclassify all records (P
tasks). After task 11, two susbsequent P tasks returned no uncertain or
positive unreviewed predictions, therefore the first session was
termined. In total 5 review tasks were required in addition to the
initial manual classification (tab. 1), for a total of 94 extra reviewed
articles (90 predicted as uncertain and 4 as positive).  
Overall 1100 classification/review actions on 1054 records were
required, that is the 12.2% of the total number of records (9016)
collected in the first session, while the rest was classified
automatically as non-relevant. The dataset at the end of the session
contained a total of 62 positive records (+11 positive records compared
to the initial manual classification).  
The ML algorithm showed a gradually improving out-of-bag performance,
ranging from 91.5% \[90.2%, 92.5%\] for the first predictive task to
94.52% \[93.94%, 94.99%\] for the last, with the sensitivity always
keeping an edge over the specificity, as per the model intentions.

| Task | Task type | Uncertain      | Positive       | Negative        | Reviewed | AUC \[98% Cr.I.\]         | Sensitivity \[98% Cr.I.\] | Specificity \[98% Cr.I.\] |
|-----:|:----------|:---------------|:---------------|:----------------|:---------|:--------------------------|:--------------------------|:--------------------------|
|    1 | R         |                | 51 (0.57%)     | 955 (10.6%)     | 1006     |                           |                           |                           |
|    2 | P         | 61 (0.68%) +61 | 42 (0.47%) +4  | 8913 (98.9%)    |          | 91.5% \[90.2%, 92.5%\]    | 86.5% \[83.3%, 89.1%\]    | 86.3% \[84.0%, 88.5%\]    |
|    3 | R         |                | 62 (0.69%) +11 | 987 (10.9%) +32 | 1049     |                           |                           |                           |
|    4 | P         | 38 (0.42%) +17 | 34 (0.38%)     | 8944 (99.2%)    |          | 94.34% \[93.70%, 95.28%\] | 91.5% \[89.2%, 94.0%\]    | 87.8% \[85.8%, 89.5%\]    |
|    5 | R         |                | 61 (0.68%) -1  | 989 (11%) +2    | 1050     |                           |                           |                           |
|    6 | P         | 46 (0.51%) +8  | 38 (0.42%)     | 8932 (99.1%)    |          | 94.47% \[93.86%, 95.17%\] | 91.1% \[89.2%, 93.2%\]    | 88.4% \[86.2%, 90.5%\]    |
|    7 | R         |                | 62 (0.69%) +1  | 991 (11%) +2    | 1053     |                           |                           |                           |
|    8 | P         | 35 (0.39%) +2  | 37 (0.41%)     | 8944 (99.2%)    |          | 94.05% \[93.41%, 95.02%\] | 90.9% \[88.9%, 92.8%\]    | 87.6% \[85.7%, 89.5%\]    |
|    9 | R         |                | 62 (0.69%)     | 991 (11%)       | 1053     |                           |                           |                           |
|   10 | P         | 47 (0.52%) +2  | 31 (0.34%)     | 8938 (99.1%)    |          | 94.22% \[93.52%, 94.76%\] | 90.9% \[88.9%, 92.7%\]    | 87.9% \[85.6%, 89.9%\]    |
|   11 | R         |                | 62 (0.69%)     | 992 (11%) +1    | 1054     |                           |                           |                           |
|   12 | P         | 31 (0.34%)     | 38 (0.42%)     | 8947 (99.2%)    |          | 94.37% \[93.54%, 94.98%\] | 91.5% \[89.1%, 93.4%\]    | 87.5% \[85.7%, 89.5%\]    |
|   13 | P         | 38 (0.42%)     | 33 (0.37%)     | 8945 (99.2%)    |          | 94.52% \[93.94%, 94.99%\] | 91.3% \[89.3%, 93.0%\]    | 88.0% \[86.3%, 89.7%\]    |

**Table 1**. Prediction (P) / review (R) tasks’ results after the first
query was performed. The Uncertain column shows the number of new
labeled records not already reviewed in the precedent tasks and the
possible increment compared to the previous P task. The Positive column
has different meaning for the type of task: for the P tasks is the
number of positevely labeled records followed by the number of new
positives not previously reviewed, if any; for the R task, the
cumulative number of positive and its increment, if any. The same
interpretation stands for the Negative column, albeit increments are not
shown for the R tasks, since the negative predictions are not reviewed.
All values are also shown as percentege over total of records available
in the session. Also, out-of-bag AUC, sensitivity and specificity are
reported, computed as decribed in the methods, with the respective 98%
Cr.I.

### Second search session
