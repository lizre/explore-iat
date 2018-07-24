# Shiny dashboard: Explore the Race & Gender Implicit Association Test data

<b>About this Shiny dashboard</b>

View the most recently published version of this dashboard <a href = "https://lizredford.shinyapps.io/explore-iat/">here</a>.

This dashboard was made by Liz Redford using the publicly-available Project Implicit <a href = "https://osf.io/y9hiq/">demo website datasets</a>. 

<a href = "https://implicit.harvard.edu/implicit/">Project Implicit</a> is a non-profit organization and international collaboration between researchers who are interested in implicit social cognition - thoughts and feelings outside of conscious awareness and control. 

This Shiny dashboard allows the user to explore scores on the Implicit Association Test, with separate tabs for the Race and Gender tests. Users can view a histogram of the scores, split those scores by participant demographics, and view correlations with significance levels of IAT scores with various demographic variables. The dashboard also includes basic About the IAT and About this Dashboard tabs.

This dashboard works best in a browser. If on mobile, it works best positioned in landscape, but still won't appear as intended.

<b>Using this code and data</b>

The code in app.R by default imports the data directly from the Github URL, but also comments out an option to download the data from this repo and then import. If you choose the latter, be sure to also run the commented line of code that recodes the "polticialid variable".