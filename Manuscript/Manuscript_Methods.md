Methods
================
2021-08-31

## Methods

### General description

We built an R (R Core Team, 2020) based framework with the aim of
simplifying two aspects of systematic reviews: record acquisition and
relevance classification. The framework is composed by several
components which act together while being independent enough to be in
principle be substituted by alternative implementations, given that the
structure of the intermediate data outputs is respected. Check
Supplemental Material S1 for an in-depth description of the framework
and how to use it.  
The tasks carried out by the framework are grouped into “sessions,” that
is a set of actions that starts from a search query with which collect a
set of unlabelled records and ends with the having a fully labelelled
set (Fig. 1). From this labelled set, the framework allows to generate a
new query and perform a new session.  
It is advisable that the research starts using the framework with a
specific query from which she expects a high relevant/non-relevant
record ratio.  
Follows a description of the framework’s components.  

![Figure 1. Framework visual depiction.](methods_diagram.png)

### Records’ acquisition

We built a set of tools to let user automatically search and download
records data from three major scientific databases: MEDLINE
(<https://pubmed.ncbi.nlm.nih.gov/>), Web Of Science (WOS,
<https://apps.webofknowledge.com/>) and the Institute of Electrical and
Electronics Engineers (IEEE,
<https://ieeexplore.ieee.org/Xplore/home.jsp>). The user needs to input
a search query and a date range. The query may contain boolean operators
AND, OR, NOT and nested parentheses. The dabatase will be also called
“sources” in the rest of the text.  
For WOS an Application Programming Interface (API) key is necessary to
use the automatic search tools; for IEEE, if an API key is not
available, a slower, webscraping-based solution will be employed; for
MEDLINE the API key is required for high frequency requests to the NCBI
server (Sayers, 2010), which may happen if the chosen query produces a
large number of records, since our tool splits a big API requests in
multiple smaller ones.  
It is also possible to download and import records in the framework
manually. This is particularly useful to acquire records from the SCOPUS
(<https://www.scopus.com/search/form.uri?display=basic#basic>) and
EMBASE databases (<https://www.embase.com/#advancedSearch/default>), for
which a comprehensive API interface was not easy to build; the framework
will be able to import the manually downloaded results seamlessly.A
short guide on how to setup the search for each supported database is
available in Supplemental Material S3.  
Once the records are dowloaded and acquired, the framework merges them
into a single database, resolving duplicates and different formatting
between sources, and ordering the records by simple query term
frequency, putting the most likely relevant on top. The output is an
“Annotation file.”

### Document-Term matrix generation

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-rstat" class="csl-entry">

R Core Team. (2020). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>

</div>

<div id="ref-pubmed" class="csl-entry">

Sayers, E. (2010). A general introduction to the e-utilities. *Entrez
Programming Utilities Help \[Internet\]. Bethesda (MD): National Center
for Biotechnology Information (US)*.

</div>

</div>
