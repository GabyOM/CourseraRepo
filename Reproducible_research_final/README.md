# Peer-graded Assignment: Course Project 2
-----------------------------------------

*Introduction*

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## **Data**
-------------
The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:

- [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

- [National Weather Service](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)  
- [National Climatic Data Center Storm Events](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)  

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

## **Review criteria**
---------------------------

    1. Has either a (1) valid RPubs URL pointing to a data analysis document for this assignment been submitted; or (2) a complete PDF file presenting the data analysis been uploaded?  
    2. Is the document written in English?  
    3. Does the analysis include description and justification for any data transformations?  
    4. Does the document have a title that briefly summarizes the data analysis?  
    5. Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?  
    6. Is there a section titled "Data Processing" that describes how the data were loaded into R and processed for analysis?  
    7. Is there a section titled "Results" where the main results are presented?  
    8. Is there at least one figure in the document that contains a plot?  
    9. Are there at most 3 figures in this document?  
    10. Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?  
    11. Does the analysis address the question of which types of events are most harmful to population health?  
    12. Does the analysis address the question of which types of events have the greatest economic consequences?     13. Do all the results of the analysis (i.e. figures, tables, numerical summaries) appear to be reproducible?  
    14. Do the figure(s) have descriptive captions (i.e. there is a description near the figure of what is happening in the figure)?  
    15. As far as you can determine, does it appear that the work submitted for this project is the work of the student who submitted it?  

# **Assignment**
--------------
The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

## *Questions*
Your data analysis must address the following questions:

    1. Across the United States, which types of events (as indicated in the **EVTYPE** variable) are most harmful with respect to population health?  
    2. Across the United States, which types of events have the greatest economic consequences?

Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.

## *Requirements*
For this assignment you will need some specific tools

    - RStudio: You will need RStudio to publish your completed analysis document to RPubs. You can also use RStudio to edit/write your analysis.
    - knitr: You will need the knitr package in order to compile your R Markdown document and convert it to HTML

## *Document Layout*

    - Language: Your document should be written in English.
    - Title: Your document should have a title that **briefly** summarizes your data analysis
    - Synopsis: Immediately after the title, there should be a synopsis which describes and summarizes your analysis in at **most 10 complete sentences.**
    - There should be a section titled Data Processing which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the **cache = TRUE** option for certain code chunks.
    - There should be a section titled Results in which your results are presented.
    - You may have other sections in your analysis, but Data Processing and Results are required.
    - The analysis document must have at least one figure containing a plot.
    - Your analysis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total.
    - You must show all your code for the work in your analysis document. This may make the document a bit verbose, but that is okay. In general, you should ensure that echo = TRUE\color{red}{\verb|echo = TRUE|}echo = TRUE for every code chunk (this is the default setting in knitr).

## **Publishing your analysis**
For this assignment you will need to publish your analysis on [RPubs.com](http://rpubs.com/). If you do not already have an account, then you will have to create a new account. After you have completed writing your analysis in RStudio, you can publish it to RPubs by doing the following:

    1. In RStudio, make sure your R Markdown document **(.Rmd)** document is loaded in the editor
    2. Click the **Knit HTML** button in the doc toolbar to preview your document.
    3. In the preview window, click the **Publish** button.
    
Once your document is published to RPubs, you should get a unique URL to that document. **Make a note of this URL** as you will need it to submit your assignment.

**NOTE:** If you are having trouble connecting with RPubs due to proxy-related or other issues, you can upload your final analysis document file as a PDF to Coursera instead.


## **Submitting Your Assignment**

In order to submit this assignment, you must copy the RPubs URL for your completed data analysis document in to the peer assessment question.

If you choose to submit as a PDF, please insert an obvious placeholder URL (e.g. https://google.com) in order to allow submission.
