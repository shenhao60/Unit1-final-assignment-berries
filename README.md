In this assignment, we do EAD for data set berries from USDA. In data cleaning process, we mainly separate mixed stings into several variables and delete variables with meaningless or replicated information. Then, according to data set attributions, we divided the whole date set into two parts:

* Bearing: a part contains information about farm chemical and fertilizer usage on berries.
* Market: a part with price, yield, area related information about berries.

For Bearing part, we use principal component analysis to explore variables' internal relationship and the results shows the PCA will not help a lot in regression.

For Market part, since the relationships between variables are extremely simple, we summarize 13 equations to explain their connections.

Besides, we deploy a shiny app [Berries-shiny](https://haoshen.shinyapps.io/Berries-shiny/) for data display and a slide [Berries-slide](Berries-slide.pdf) to presente the results. And the document recored the whole processes is in [Berries-rmd](Berries-rmd.pdf).