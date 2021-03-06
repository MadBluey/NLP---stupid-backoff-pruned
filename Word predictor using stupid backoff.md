Word predictor using the stupid backoff algorithm
========================================================
author: Karl Kaspar Haavel
date: 26.11.17
autosize: true

Overview
========================================================

The goal of this project was to create a word prediction algorithm using the data used in the capstone coursera project.

The following steps were implemented to achieve this goal:

- getting and cleaning the data, 
- exploratory data analysis,
- prototyping,
- prediction algorithm,
- and in the end the shiny web application 


Predictive algorithm
========================================================

The predictive algorithm can be cut down to these steps:

- Text is input.
- The input is chopped up and cleaned.
- We ignore the main part of the sentence and use only the last few words (Zipf's law). 
- Words will filter the n-grams which were created from the data provided. 
- Words will be sorted using the stupid back-off scoring algorithm. 
- Output is forwarded to the Shiny web application

Shiny web application
========================================================

The shiny web application does the following things: 

- Takes the users input
- Provides a table with the predicted next words and their scores. 
- Those words are presented also in a word cloud where their size is equal to their score.

Shiny web app 


Thank you
========================================================

- Stupid backoff model -> 
- Zipf's Law -> 
- Shiny web app -> 
