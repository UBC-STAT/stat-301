## Group Project (Case Study)

As in DSCI 100 and STAT 201, students will work in groups to complete a Data Science project from the beginning
(downloading data from the web) to the end (communicating their methods and conclusions in an electronic
report). The electronic report will be a Jupyter notebook in which the code cells will download a dataset from the web, reproducibly and sensibly wrangle and clean, summarize and visualize the data, as well as fitting and selecting a model. Throughout the document, markdown cells will be used to communicate the question asked, methods used, and the conclusion reached.

For this project, you will need to formulate formulate a research question of your choice, 
and then identify and use a dataset to answer to the question. We list some suggested data sets below; however, we encourage you to use another data set that interests you. If you are unsure whether your dataset is adequate, please reach out to a member of the
teaching team.

**Important Note:** Although this is a group project, some related assignments will be submitted individually. You can (and are encouraged to) discuss it with your group members. However, *every* student will submit their own assignment and will receive an individual grade. 

**Submission:** All assignments must be done in a Jupyter notebook, and then submitted both as an `.html` file (`File` &#8594; `Download As` &#8594; `HTML`) and an `.ipynb` file that is reproducible (i.e. works and runs without any additional files).

### Deliverable 1: Team Contract (1% weight)

A **group contract** is a document to help you formalize the expectations you have for your group members and what they can expect of you. It will help you think about what you need from each other to work effectively as a team! You will create and agree on this contract as a team. **Each member should "sign" (you can just type out your name) at the bottom of the submission**. At a minimum, your group contract must address the following:

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

#### Data:

Many data analyses start with a question or questions we are intrested in. Then we find or collect data to answer the question(s). Decide with your team members a topic of interest you want to work on and identify a dataset to work with. Include a brief description of the dataset (2 or 3 sentences) and a link or reference on how to access the dataset. Note that in the next individual assignment you will be asked to fully describe the dataset.


### Deliverable 2-4: Individual assignments (total 12% weight, 4% each assignment)

Although this is a group project, the following 3 assignments related to a dataset chosen by the group will be submitted individually. You can (and are encouraged to) discuss them with your group members. However, you don't need to come with a common solution and *every* student has submit their own assignment and will receive an individual grade. 

#### Assignment 1: Data and Question(s)

*This is an individual assignment*. Every student needs to write and submit their own assignment. You must submit **two files** in Canvas:

- the source Jupyter notebook (`.ipynb` file)
- the rendered final document (`.html` file)

#### Data:

In the contract you identified a dataset to work with your group. In this assignment, provide a full description of the dataset chosen. 

Note that the selected dataset will probably contain more variables than you need. In fact, exploring how the different variables in the dataset affect your model may be crucial part of the project. Regardless of which variables you plan to use, provide a full descriptive summary of the dataset. This is *not* an Exploratory Data Analysis, just a characterization of the data. Include information such as: number of observations, number of variables, name and type of variables, etc. You may want to use a table or bullet points to describe the variables in the dataset.

Include a brief description of the dataset indicating how the data has been collected or where it comes from.

#### Question:

Clearly state the question you will try to answer using the selected dataset. Your question should involve one random variable of interest (the response) and one or more explanatory variables. Describe clearly how the data will help you address the question of interest. Explain whether your question is focused on prediction, inference, or both.

It is fine to have the same question as other group members. However, you don't need to agree on a unique common question for the group project. In fact, usually many questions can be answered with the same dataset. Regardless of how many questions are proposed within each group, *each team member* needs to state and justify a question of interest. 

#### Assignment 2: Exploratory Data Analysis and Visualization

In this assignment, you will:

- Demonstrate that the dataset can be read from the web into R.
- Clean and wrangle your data into a tidy format.
- Propose a visualization that you consider relevant to address your question or to explore the data.
  - propose a high quality plot or set of plots of the same kind (e.g, histograms of different variables)
  - explain why you consider this plot relevant to address your question or to explore the data 

**Note**: this visualization does not have to illustrate the results of a methodology. Instead, you are exploring which variables are relevant, potential problems that you anticipate to encounter, groups in the observations, etc. 

It is fine to share ideas with other group members. However, you don't need to agree on a unique common visualization for the group project. In fact, usually the exploratory data analysis will have many different visualizations! Regardless of how many plots are proposed within each group, *each team member* needs to propose one visualization and justify their choice. 

#### Assignment 3: Methods and Plan

Propose one method to address your question of interest using the selected dataset and explain why it was chosen. In your explanation respond to the following questions:
- Why is this method appropriate?
- Which assumptions are required, if any, to apply the method selected?
- What are potential limitations or weaknesses of the method selected?

As mentioned before, it is fine to share ideas with other group members. However, you don't need to agree on a unique common method for the group project. In fact, usually the analysis comprise different methods with different strengths and limitations! Regardless of how many methods are proposed within each group, *each team member* needs to propose one method and justify its choice. 

### Deliverable 5: Interview with your TA (4% weight)

The whole group will meet for 5 minutes with the TA to suggest a plan on how to combine all the material in a final report. Students will be graded individually. Your grade will be based on your participation in the interview and the quality of your arguments and explanations. 

### Deliverable 6: Individual assignment (4% weight)

#### Assignment 4: Computational Code and Output 

This will be the last individual assignment. Write a computation code to implement the method proposed in the previous assignment (or suggested in the interview) and use a visualization or table to report the results. In 3 or 4 sentences give a brief interpretation of the results. If needed, comment on any unexpected result or potential problems with the analysis, and possible ways to address issues encountered. If results are as expected, explain how they address the question of interest.

### Deliverable 7: Group Final Report (5% weight)

Each group will create a final electronic report (max 2000 written words, not including citations) using Jupyter to communicate the
question asked, the analysis performed and the conclusion reached.

Only one member of your team needs to submit. The group will work on combining the material from all individual assignments. The way in which the material is combined will depend on each project. You don't need to include everything done. Just make sure that all team members agree on the final content of the report. Discuss options with your TA and Instructor if needed.

You must submit **two files**:

- the source Jupyter notebook (`.ipynb` file)
- the rendered final document (`.html` file)
  
Each report should include the following sections:
 - Title
 - Introduction
 - Methods and Results
 - Discussion
 - References

#### Introduction

Begin by providing some relevant background information on the topic so
that someone unfamiliar with it will be prepared to understand the rest
of your proposal. 

Propose one or two questions you want to examine and describe the dataset that will be used to answer the
question(s). 

Also, be sure to frame your question/objectives in terms of what is
already known in the literature. Be sure to include at least two
scientific publications that can help frame your study (you will need to
include these in the References section).

#### Methods and Results

In this section, you will include:

a) "Exploratory Data Analysis (EDA)"
- Demonstrate that the dataset can be read from the web into R.
- Clean and wrangle your data into a tidy format.
- Plot the relevant raw data, tailoring your plot in a way that addresses your question.
  - make sure to explore the association of the explanatory variables with the response.
  - your Exploratory Data Analysis (EDA) must be comprehensive with high quality plots. 
- Any summary tables that is relevant to your analysis.

Be sure to not print output that takes up a lot of screen space.

b) “Methods: Plan” 

- Describe in written English the methods you used to perform your
analysis from beginning to end that narrates the code that does the
analysis.
- If included, describe the "Feature Selection" process, how and why you choose the covariates of your final model.
- Make sure to interpret/explain the results you obtain. It’s not enough to just say "I fitted a linear model with these covariates, and my R-square is 0.87".
  - if inference is the aim of your project, detailed interpretation of your fitted model is required, as well as a discussion of relevant quantities (e.g., are the coefficients significant? how is the model fitting the data)?
  - a careful model assessment must be conducted.
  - if prediction is the aim of the project, describe the test data used or how it was created.
- Ensure your tables and/or figures are labeled with a figure/table number.

#### Discussion

In this section, you’ll interpret the results you obtained in the previous section with respect to the main question/goal of your project.

- Summarize what you found, and the implications/impact of your findings.
- If relevant, discuss whether your results were what you expected to find.
- Discuss how your model could be improved;
- Discuss future questions/research this study could lead to.

#### References

At least two citations of literature relevant to the project. The citation
format is your choice – just be consistent. Make sure to cite the source
of your data as well.

### Deliverable 8: Team Evaluation (2% weight)

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

Part of these instructions come from descriptions of the DSCI 100 and STAT 201 projects at UBC.
