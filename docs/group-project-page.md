## Group Project

As in DSCI 100 and STAT 201, students will work in groups to complete a Data Science project from the beginning
(downloading data from the web) to the end (communicating their methods and conclusions in an electronic
report). The electronic report will be a Jupyter notebook in which the code cells will download a dataset from the web, reproducibly and sensibly wrangle and clean, summarize and visualize the data, as well as fitting and selecting a model. Throughout the document, markdown cells will be used to communicate the question asked, methods used, and the conclusion reached.

For this project, you will need to formulate formulate a research question of your choice, 
and then identify and use a dataset to answer to the question. We list some suggested data sets below; however, we encourage you to use another data set that interests you. If you are unsure whether your dataset is adequate, please reach out to a member of the
teaching team.

### Deliverable 1: Team Contract

A group contract is a document to help you formalize the expectations you have for your group members and what they can expect of you. It will help you think about what you need from each other to work effectively as a team! You will create and agree on this contract as a team. **Each member should "sign" (you can just type out your name) at the bottom of the submission**. At a minimum, your group contract must address the following:

#### Goals

- What are our team goals for this project?
- What do we want to accomplish?
- What skills do we want to develop or refine?
- Expectations
- What do we expect of one another regarding attendance at meetings, participation, frequency of communication, quality of work, etc.?

#### Policies & Procedures

What rules can we agree on to help us meet our goals and expectations?

#### Consequences:

How will we address non-performance regarding these goals, expectations, policies and procedures?

### Deliverable 2: Project Proposal

Each group is expected to prepare a written proposal within 500 words
(about 1 page) with a research question of interest, and the dataset they plan to use. 
The proposal should be done in a Jupyter notebook, and
then submitted both as an `.html` file (`File` &#8594; `Download As` &#8594; `HTML`) and
an `.ipynb` file that is reproducible (i.e. works and runs without any additional files).

Only one member of your team needs to submit. You must submit **two files**:

- the source Jupyter notebook (`.ipynb` file)
- the rendered final document (`.html` file)

Each proposal should include the following sections:

- Title
- Introduction
- Exploratory Data Analysis
- Methods: Plan
- References

#### Introduction

Begin by providing some relevant background information on the topic so
that someone unfamiliar with it will be prepared to understand the rest
of your proposal.

Clearly state the question you will try to answer with your project.
Your question should involve one random variable of interest (the response) and one or more explanatory variables. 
Of the response variable, explain whether your project is focused on prediction, inference, or both.

Identify and describe the dataset that will be used to answer the
question. Remember, this dataset is allowed to contain more variables
than you need, in fact, exploring how the different variables in the dataset affect your model, 
is a crucial part of the project.

Also, be sure to frame your question/objectives in terms of what is
already known in the literature. Be sure to include at least two
scientific publications that can help frame your study (you will need to
include these in the References section).

#### Preliminary Results

In this section, you will:

- Demonstrate that the dataset can be read from the web into R.
- Clean and wrangle your data into a tidy format.
- Plot the relevant raw data, tailoring your plot in a way that addresses your question.
  - make sure to explore the association of the explanatory variables with the response.
  - your Exploratory Data Analysis (EDA) must be comprehensive with high quality plots. 
- Any summary tables that is relevant to your analysis.

Be sure to not print output that takes up a lot of screen space.

#### Methods: Plan

The previous sections will carry over to your final report (you’ll be
allowed to improve them based on feedback you get). Begin this *Methods*
section with a brief description of “the good things” about this report
– specifically, in what ways is this report trustworthy?

Finish this section by reflecting on how your final report might play out:

- What methods do you plan on using?
- What do you expect to achieve?
- What impact could your results have?

#### References

At least two citations of literature relevant to the project. The citation
format is your choice – just be consistent. Make sure to cite the source
of your data as well.

### Deliverable 3: Peer Review

For this peer review, your group will provide feedback to
another group’s proposal. While initially this was planned to be an individual assignment, it has been changed to a group assignment to accommodate all technical problems we are experiencing.

#### Instructions

To complete the peer-review assigned to your group, write comments in Canvas' text box or through an attached file. Recall that you'll be evaluated in the comments and feedback given. See the rubric added to the Peer Review assignment in Canvas.

Remember to be _respectful and provide constructive comments_. 

Following a recommendation to reviewers of BMC Genomics Journal: "Before you submit your report, please take a moment to read it through and put yourself in the place of the authors. How would you feel if you received this report? Would you find the tone courteous and professional?" Are your comments useful to strength other groups' analysis?

**General guideline**
Your review should contain the following elements:

- As you read through the proposal, point out anything that you think is confusing, or is not
communicated effectively. When possible, provide suggestions for improvement. If everything looks
good to you, say why it looks good.
- What part of the proposal is the most effective, and why?
- What part of the proposal is the least effective, and why? Provide a suggestion for improvement.
- Provide feedback on English, spelling, and grammar (if applicable).

The rubric can be found on Canvas. In short, *mechanics* (10%) evaluates
the composition of your submission, *reasoning* (70%) evaluates your
feedback, and *writing* (20%) evaluates your English, spelling, and
grammar. There is no page limit. 

### Deliverable 4: Final Report

Each group will create a final electronic report (max 2000 written
words, not including citations) using Jupyter to communicate the
question asked, the analysis performed and the conclusion reached.

Only one member of your team needs to submit. You must submit **two files**:

- the source Jupyter notebook (`.ipynb` file)
- the rendered final document (`.html` file)
- Each report should include the following sections:
  - Title
  - Introduction
  - Methods and Results
  - Discussion
  - References

#### Introduction

The instructions for this section are the same in your proposal. Just be
sure to improve this section by incorporating feedback, and changing
things based on your own improved understanding of the project (now that
more time has passed since the proposal).

#### Methods and Results

Here is where you’ll include your work from the “Preliminary Results” in
your proposal, along with the additional results you planned to conduct,
as indicated in the “Methods: Plan” section of your proposal. Be sure to
incorporate feedback from the teaching team and your peers (as
relevant), or make any improvements based on your own improved
understanding of the project (now that more time has passed since the
proposal).

Specifically, in addition to what is requested in the “Preliminary
Results” section of the proposal, we are looking for the following
components:

- Describe in written English the methods you used to perform your
analysis from beginning to end that narrates the code the does the
analysis.
- the "Feature Selection" process, how and why you choose the covariates of your final model.
- Make sure to interpret/explain the results you obtain. It’s not enough to just say "I fitted a linear model with these covariates, and my R-square is 0.87".
  - if inference is the aim of your project, detailed interpretation of your fitted model is required, as well as a discussion of relevant quantities (e.g., are the coefficients significant? how is the model fitting the data)?
  - a careful model assessment must be conducted.
- Ensure your tables and/or figures are labeled with a figure/table number.

#### Discussion

In this section, you’ll interpret the results you obtained in the previous section with respect to the main question/goal of your project.

- Summarize what you found, and the implications/impact of your findings.
- If relevant, discuss whether your results were what you expected to find.
- Discuss how your model could be improved;
- Discuss future questions/research this study could lead to.


#### References

The same instructions for your proposal also applies here. You only need to make changes if necessary (e.g., if feedback indicates so).

### Deliverable 5: Team Evaluation

Evaluate each member of your group (including yourself) in terms of how they/you participated,
prepared, helped the group excel, and was a team player.

Click the Teammate Evaluation Template link in the Canvas home page to access the teamwork document. Fill out the jupyter notebook, then download an html rendering of the completed notebook by going to `File` &#8594; `Download As` &#8594; `HTML`. Finally, submit the rendered HTML document here.

- This is **not** a group submission. Every member of the group must complete and submit this individually.
- This is **not** a standard worksheet/tutorial. You **must** download off our server and submit the html to Canvas.

### Data
- [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets.php) has hundreds of datasets
- City of Vancouver data: [https://opendata.vancouver.ca/pages/home/](https://opendata.vancouver.ca/pages/home/) (or other cities, like Ottawa, Toronto, etc.)
- Vancouver crime data: [https://geodash.vpd.ca/opendata/](https://geodash.vpd.ca/opendata/)
- BC data: [https://data.gov.bc.ca/](https://data.gov.bc.ca/)
- Water survey of Canada: [https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey.html](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey.html)
- Climate and Weather Data: [https://climate.weather.gc.ca/](https://climate.weather.gc.ca/)
- Sports data

### Attribution

These instructions are a modified version of the DSCI 100 and STAT 201 project at UBC.
