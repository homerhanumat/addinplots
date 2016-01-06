# addinplots

## Introduction

This is a package of Addins.  For more about Addins and how to use them, please see
[this article](https://support.rstudio.com/hc/en-us/articles/215605467).

Each Addin in this package is a code-helper for a particular type of plot in the `lattice` graphing system.  The intention is to help students and other newcomers to `lattice` to make reasonably well-customized graphs while teaching (through example) the rudiments of the coding principles of the `lattice` package.


## Installation

Follow the installation directions in the article cited above.  In addition, install this package and one of its dependencies:

```
devtools::install_github("homerhanumat/shinyCustom")
devtools::install_github("homerhanumat/addinplots")
```

## Use

Type the name of a data frame into the console or an R script,or inside a code chunk in an R Markdown document.  Select the name.  Then go to the Addins button and pick the Addin for the plot you wish to make.  The Addin will walk you through the process of constructing a graph based upon variables in your data frame.  At each step you see the graph to that point along with R-code to produce said graph.  When you are happy with your graph press the Done button.  The app will go dark.  Close the app tab and return to RStudio.  You will see that the code for your graph has been inserted in place of the name of the data frame.

These Addins are flexible enough to handle the everyday needs of beginning students in undergraduate statistics classes, but they only scratch the surface of `lattice`'s capability.  Eventually you should graduate to coding directly with `lattice`.

*Note on Use*:  The Addins are intended for use in classroom settings, where the entire class is working on a not-so-powerful RStudio server.  Accordingly many of the input controls have been customized to inhibit their propensity to update.  When you are entering text or a number, you need to press Enter or shift focus away from the input area in order to cue the machine to update your information.  You will also note that sliders take a bit longer to "respond".  This behavior prevents the Server from being deluged by numerous requests for expensive graph-computations that most users don't intend.

