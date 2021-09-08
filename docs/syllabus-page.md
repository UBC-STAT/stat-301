## Syllabus

Explanatory and predictive data analysis with multiple explanatory variables. Choosing the right methods to apply based on the statistical question and data at hand. Trade-offs between model-based and non-model based approaches. Emphasis placed on case studies and real data sets, as well as reproducible and transparent workflows when writing computer scripts for analysis and reports.

### COVID-19 Statement
Please read the [COVID-19 instructions](https://ubc-stat.github.io/stat-301/covid-safety.html).

### Prerequisites
- STAT 201: Statistical Inference for Data Science
- One of MATH 100, MATH 102, MATH 104, MATH 110, MATH 120, MATH 180, MATH 184, SCIE 001.
- Access to a computer.
    - If a student does not have their own laptop or chromebook, students may be able to [loan a laptop from the UBC library](https://services.library.ubc.ca/computers-technology/technology-borrowing/).

### When and where?
- The lectures will be on Tuesdays and Thursdays from 12:30 to 14:00.
- The lectures will be held in Henry Angus building, Room 234. 
- Office hours will be held on Wednesdays 3:30pm via Zoom (link on Canvas).

### Software Platforms
- Students will learn to perform their analysis using the [R programming language](https://cran.r-project.org/).
- Worksheets and tutorial problem sets as well as the final project analysis, development, and reports will be done using [Jupyter Notebooks](http://jupyter.org/).
- Students will access the worksheets and tutorials in Jupyter Notebooks through Canvas. 

### Textbooks
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning: With applications in R. 
    - Available ONLINE at UBC Library: http://resolve.library.ubc.ca/cgi-bin/catsearch?bid=6667014
- Rafael Irizarry. Introduction to Data Science. 
    - Available ONLINE at https://rafalab.github.io/dsbook/

- **Modern Dive:** we used this book in STAT 201. This books also has some useful material for this course, in particular:
    - [Chapter 5: Basic Regression](https://moderndive.com/5-regression.html)
    - [Chapter 6: Multiple Regression](https://moderndive.com/6-multiple-regression.html)
    - [Chapter 10: Inference for Regression](https://moderndive.com/10-inference-for-regression.html)


### Learning Outcomes
By the end of the course, students are expected to be able to:

- Describe real-world examples of explanatory modelling (e.g. A/B testing optimization & regression with variable selection) and predictive modelling problems.

- Explain the trade-offs between model-based and non-model based approaches, and describe situations where each might be the preferred approach.

 - Explain the difference between creating models for explanation vs prediction, in the context of both how you choose and evaluate models as well as how you interpret the results.

- Choose & apply a suitable method  (e.g., regression, GLM's, sample size estimation, controlling for multiple testing, peeking, bandit algorithms, variable selection, model diagnostics)  based on the statistical question and data at hand. Discuss the advantages and disadvantages of different methods that may be suitable for a given problem.

- Correctly interpret computer output when performing the statistical analyses presented in this course, in the context of the statistical question being asked and the audience being reported to. 
 
-  Identify the assumptions / conditions required for each method to produce reliable results. Choose techniques to check (or at least be able to falsify) those assumptions. Discuss the consequence(s) of mapping the wrong methods to the question and/or data type.


### Assessments
Each week there will be two assignments: (1) a worksheet; and (2) a tutorial.
These assignments will be due every Saturday, 23:59:59. The worksheets are
fully autograded with visible tests to help you identify points that need
more clarification. Therefore, reach out to the teaching team if you don't
understand why you are getting an answer wrong in the worksheet. On the
other hand, the tutorials are not fully autograded, and only a few exercises
will have visible tests. You can access the assignments through Canvas ([assignments](https://canvas.ubc.ca/courses/89390/assignments)).

To submit your assignment, make sure your work is saved
**on our server** (i.e., using the link from Canvas) before the deadline.
Our server will automatically snapshot at the due date/time. Also, please **do not** rename the assignments files.

#### Assessments' Weights

<div style="display: flex; height: max-content;">
    <table>
        <caption>Table 1: Course</caption>
        <thead>
            <tr>
                <th>Deliverable</th>
                <th>Weight</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Worksheets</td>
                <td>5%</td>
            </tr>
            <tr>
                <td>Tutorials</td>
                <td>15%</td>
            </tr>
            <tr>
                <td>Group project</td>
                <td>20%</td>
            </tr>
            <tr>
                <td>Midterm 1</td>
                <td>15%</td>
            </tr>
            <tr>
                <td>Midterm 2</td>
                <td>15%</td>
            </tr>
            <tr>
                <td>Final Exam</td>
                <td>30%</td>
            </tr>
        </tbody>
    </table>

    <table>
        <caption>Table 2: Project</caption>
        <thead>
            <tr>
                <th>Deliverable</th>
                <th>Weight</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Team Work Contract</td>
                <td>2%</td>
            </tr>
            <tr>
                <td>Proposal</td>
                <td>4%</td>
            </tr>
            <tr>
                <td>Peer review</td>
                <td>3%</td>
            </tr>
            <tr>
                <td>Final Report</td>
                <td>9%</td>
            </tr>
            <tr>
                <td>Teammate Evaluation</td>
                <td>2%</td>
            </tr>

        </tbody>
    </table>
</div>

#### Test Dates

- Midterm 1: Thursday, October 7 at 12:30pm
- Midterm 2: Tuesday, November 2 at 12:30pm
- Final Exam: TBD.

#### Exams
All the exams will be open-book. You can use whatever (<em>but not whoever</em>) you want. You **are not allowed** to communicate with another person during the exams. For all the exams, the platform used will be Canvas. The types of questions can vary: reasoning, multiple-choice, multiple-answer, dropdown, true or false. Although most questions will be about the content, you can expect a few coding questions. That being said, the coding question will not be overly complicated, and we will only check your familiarity with the main functions and packages we use in the course. We **are not** trying to test your memory!!! Please don't spend energy trying to memorize everything. If you had done the worksheets and tutorial, this should not be a problem for you.

- **Midterms:** two 45-minute mid-terms will be administered at the start of the lecture in Weeks 5 and 9 (or Weeks 3 and 5 in Summer Terms). The first midterm will cover (roughly) Weeks 1 to 4 (i.e., Worksheets/Tutorials 1, 2, 3, and 4, plus the readings). The second midterm will focus (roughly) on Weeks 6, 7, and 8 (i.e., Worksheets/Tutorials 1, 2, 3, and 4, plus the readings), however, the content is cumulative as the concepts are dependent. Note that part of the material covered may be excluded in specific midterms. The instructors will give and post more precise information before each exam.  

- **Final Exam:** The final exam will be a two-hour exam and it will cover the material of
the **entire course**. Notwithstanding the grading weights presented above, <em>students must pass the final exam</em> to pass the course.

### Policies

#### Late/Absence
- Please, read the [COVID-19 instructions](https://ubc-stat.github.io/stat-301/covid-safety.html) document for absences related to COVID-19.
- Regular attendance to lecture and tutorials is expected of students. Students who are unavoidably absent because of illness or other reasons should inform the instructor(s) of the course as soon as possible, preferably, prior to the start of the lecture/tutorial. 
- There will be no make-up exams. Students who miss Midterm 1, Midterm 2 or an assignment and want to request an Academic Concession need to contact the Instructor as soon as possible and provide a self-declaration form (available on canvas). Failing to present a declaration may result in a grade of zero.
- Late submissions of *worksheets* and *tutorials* will receive a grade of 0. 
- For other assessments, late submission is defined as any work submitted after the deadline. Late submissions will receive a 50% deduction penalty of the original grade in the first occurrence if submitted within 48 hours of the deadline. Hence a maximum attainable grade for the first piece of work submitted late is 50%. Any additional pieces of work that are submitted late will receive a grade of 0 for subsequent occurrences. Any submission after 48 hours of the original deadline will receive a grade of 0.

#### Excused assignment 
- We are aware that sometimes life gets in the way of getting things done. For example, you might feel sick in a given week, have a family emergency, or feel overwhelmed with your other courses. For this reason, we will drop the lowest grade lab, quiz and webwork (one of each) at the end of the semester. This will be done automatically, so you don't need to let us know

#### Autograder Policy 
Many of the questions in assignments are graded automatically by software. The grading computer has exactly the same hardware setup as the server that students work on. No assignment, when completed, should take longer than 5 minutes to run on the server. The autograder will automatically stop (time out) for each student assignment after a maximum of 5 minutes; *any ungraded questions at that point will receive a score of 0*.


Furthermore, students are responsible for making sure their assignments are *reproducible*, and run from beginning to end on the autograding computer. In particular, <em>please ensure that any data that needs to be downloaded is done so by the assignment notebook with the correct filename to the correct folder.</em> A common mistake is to manually download data when working on the assignment, making the autograder unable to find the data and often resulting in an assignment grade of 0. Even small mistakes such as using the wrong sample size will justify an incorrect response for that (and possibly downstream) questions.

**In short: whatever grade the autograder returns after 5 minutes (assuming the teaching team did not make an error) is the grade that will be assigned.**

*Tip: when you're done the assignment, click "Restart and Run All", and check that the autograder returns the results you are expecting.*

#### Regrading
If you have concerns about the way your work was graded, please open a request within one week of having the grade returned to you. After this one-week window, we may deny your request for re-evaluation. Also, please keep in mind that your grade may go up or down as a result of re-grading. To open a regrade requests, please follow the steps below:
1. Go to Piazza and click on `New post`.
2. In `Post Type`, select `Question`.
3. Make the post private to instructors and TAs only. In `Post to` select `Individual Students(s)/Instructor(s)`. A text box will appear, where you must type `Instructors`.
4. In `Select Folder(s)` select the folder  `regrading`.
5. In `Summary` say the Assignment you want to be regraded, followed by the question and your name and student number. For example, `lab 3 -> Q3 -- Rodolfo Lourenzutti (9982313)`
6. Provide a brief reason for why the regrade is needed.
7. The TAs will see the request and will take a look at the assignment. If necessary, they will involve the instructors. Finally, once the TA is finished reassessing the assignment:
    - If the grade deserves more marks: the TA will update the mark on Canvas and comment on the question so everyone can see that the question has been addressed.
    - If your grade goes down or stays the same: the TA will answer the post on Piazza, giving the student a reason for their final decision.



#### Device/Browser
Students are responsible for using a device and browser compatible with all functionality of Canvas. Chrome or Firefox browsers are recommended; Safari has had issues with Canvas quizzes in the past.

#### Missed Final Exam 
Students who miss the final exam must report to their faculty advising office within 48 hours of the missed exam, and must apply for deferred standing: https://students.ubc.ca/enrolment/academic-learning-resources/academic-advising. Only **your faculty advising office** can grant deferred standing in a course. You must also notify your instructor prior to (if possible) or immediately after the exam. 

If you're a <em>Science student</em>, you must apply for deferred standing (an academic concession) through Science Advising no later than 48 hours after the missed final exam/assignment. Learn more and find the application online:https://science.ubc.ca/students/advising/concession.

Students who are granted deferred standing write the final exam/assignment at a later date.Your instructor will let you know when you are expected to write your deferred exam. Deferred exams will ONLY be provided to students who have applied for and received deferred standing from their faculty.

#### Academic Concession Policy
Please see [UBC's concession policy](http://www.calendar.ubc.ca/vancouver/index.cfm?tree=3,329,0,0) for detailed information on dealing with missed coursework, quizzes, and exams under circumstances of an acute and unanticipated nature.

#### Academic Integrity 
The academic enterprise is founded on honesty, civility, and integrity. As members of this enterprise, all students are expected to know, understand, and follow the codes of conduct regarding academic integrity. At the most basic level, this means submitting only original work done by you and acknowledging all sources of information or ideas and attributing them to others as required. This also means you should not cheat, copy, or mislead others about what is your work. Violations of academic integrity (i.e., misconduct) lead to the breakdown of the academic enterprise, and therefore serious consequences arise and harsh sanctions are imposed. For example, incidences of plagiarism or cheating may result in a mark of zero on the assignment or exam and more serious consequences may apply if the matter is referred to the President's Advisory Committee on Student Discipline. Careful records are kept in order to monitor and prevent recurrences.

A more detailed description of academic integrity, including the University's policies and procedures, may be found in the [Academic Calendar](http://calendar.ubc.ca/vancouver/index.cfm?tree=3,54,111,0).


#### Plagiarism 
Students must correctly cite any code or text that has been authored by someone else or by the student themselves for other assignments. Cases of plagiarism may include, but are not limited to:
1. the reproduction (copying and pasting) of code or text with none or minimal (e.g., changing the name of the variables).
2. the translation of an algorithm or a script from a language to another.
3. the generation of code by automatic code-generation software.

An “adequate acknowledgement” requires a detailed identification of the (parts of the) code or text reused and a full citation of the original source code that has been reused.

The above attribution policy applies only to assignments. **No code or text may be copied (with or without attribution) from any source during a quiz or exam**. Answers must always be in your own words. At a minimum, copying will result in a grade of 0 for the related question.

Repeated plagiarism of any form could result in larger penalties, including failure of the course.

### Attribution 
This syllabus was copied almost in its entirety from the [DSCI 100 syllabus](https://github.com/UBC-DSCI/dsci-100/blob/master/README.md), with only minor modifications.
Parts of DSCI 100 syllabus, hence of this syllabus, have been copied from the [UBC MDS Policies](https://ubc-mds.github.io/policies/).
