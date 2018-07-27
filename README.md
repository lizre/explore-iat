# Shiny dashboard: Explore the Race & Gender Implicit Association Test data

<b>About this Shiny dashboard</b>

View the most recently published version of this dashboard <a href = "https://lizredford.shinyapps.io/explore-iat/">here</a>.

This dashboard was made by Liz Redford using the publicly-available Project Implicit <a href = "https://osf.io/y9hiq/">demo website datasets</a>. 

<a href = "https://implicit.harvard.edu/implicit/">Project Implicit</a> is a non-profit organization and international collaboration between researchers who are interested in implicit social cognition - thoughts and feelings outside of conscious awareness and control. 

This Shiny dashboard allows the user to explore scores on the Implicit Association Test, with separate tabs for the Race and Gender tests. Users can view a histogram of the scores, split those scores by participant demographics, and view correlations with significance levels of IAT scores with various demographic variables. The dashboard also includes basic About the IAT and About this Dashboard tabs.

This dashboard works best in a browser. If on mobile, it works best positioned in landscape, but still won't appear as intended.

<b>Using this code and data</b>

The code in app.R by default imports the data directly from the Github URL, but also comments out an option to download the data from this repo and then import. If you choose the latter, be sure to also run the commented line of code that recodes the "polticialid variable".

<b>Reproducing the datasets in this repo
  <i>Race</i>
  SPSS syntax (.sps file) was used to clean and take a random sample of the Project Implicit publicly-available datasets to create a dataset small enough to be handled by Shiny. These smaller datasets are the ones posted here in this repo. The SPSS syntax also cleans the original publicly-available dataset; it was partly developed to clean the Project Implicit datasets as part of a <a href = "https://osf.io/rfzhu/">project*</a> led by Kate Ratliff, and much of the syntax was developed by Jenny Howell.

<i>Gender-Science</i>
  gendersci_clean_sample.R was used to take certain variables from, and a random sample of, data from the Gender-Science Implicit Association Test. This data had already been cleaned by Hila Zitelny as part of the same Documenting Bias project.

* Full project title: Pervasiveness and Correlates of Implicit Attitudes and Stereotypes II: 2008-2015. <a href = "https://osf.io/rfzhu/">Click here for all code and data.</a>
