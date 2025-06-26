# Reporting Data Dashboard
A R Shiny dashboard to automatically prepare graphs, gather summary statistics, and perform statistical tests for a 360-feedback platform. Users select themes of questions called "Cells" and review themselves on a scale of 1-10, then invite reviewers to rate them on the behaviours, called "Questions". This demo uses a synthetic dataset generated using the `dataset.ipynb` file and uses simple numeric labels e.g. Cell 1, Cell 2, Cell 3, Question 1, Question 2, Question 3 etc. The dataset includes 3 fictional organisations with 3 populations (or departments) in each. Each population contains 3 surveys (or the set of questions a user engages with at one time). Every population has a distinct subset of all users, all users in a population are included in every survey. All users have invited reviewers who are either 'line managers' or 'others.' User demographics are also randomly generated, full details available in `dataset.ipynb`

## Get Started
Select an organisation or population ID from the dropdowns in the sidebar. Fill in the remaining filters if you wish. Navigate functions using the tabs, use the "?" icon for more info.

Graphs are provided under the "Report Graph" boxtabs, expand to fill the inputs and click on "Draw Graph" to create the plot. Customise plots using the gear icon on the top right, customise inputs using the input selections. *Note: you must click on "Draw Graph" after any modifications to view them.*

Reporting functions are divided into tabs:
* **User Scores**: View the distribution of the average score each user received from their reviewers between cells and questions. Report Graphs contains boxplot graphs and Cleveland dot plots.
* **Cells**: View average and measures of dispersion of Cell scores, along with bar graphs by relationship to user. Report Graphs contains bar graphs.
* **Cells & Questions**: View average and measures of dispersion of Behaviour scores, along with bar graphs by relationship to user. Report Graphs contains bar graphs.
* **Variability Graphs**: Dispersion of scores density plots by relationship to user.
* **Time Series**: Change in scores over time with scatter plots. *Note: these may look weird due to the synthetic data generation.*
* **Demographics**: Breakdown of scores by demographics and by user/reviewer. Report Graphs contains treemap plots.
* **Demographic Differences**: Difference in scores by cell/behaviour between demographic groups by user/reviewer. Includes statistical significance testing using Kruskal-Wallis, post-hoc Dunn test, and custom grouping of demographics. Report Graphs include stacked bar charts which will use custom grouping or if none specified, default demographic grouping.
