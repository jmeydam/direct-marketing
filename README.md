# Finding Opportunity in Crisis

This is the second capstone project for 
[HarvardX: PH125.9x](https://www.edx.org/professional-certificate/harvardx-data-science) 
on [edX](https://www.edx.org).

All results are derived from the data set and the accompanying documentation.

Instructor edX course: [Prof. Rafael Irizarry](https://www.hsph.harvard.edu/rafael-irizarry/).

## Executive Summary

This is an analysis of the [bank marketing data set](http://archive.ics.uci.edu/ml/datasets/Bank+Marketing) 
made available via the [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml) 
maintained by the University of California, Irvine. 

The data were collected during direct marketing campaigns run by a Portuguese 
bank. The classification goal is to predict whether a client will accept the 
offer - made via phone call - to invest money in a term deposit.

Note that the data were collected from May 2008 to November 2010, covering the
period from just before until two years into the financial crisis, and that 
term deposits are considered a safe investment.

The date of the observations is not given, but the 41,188 observations are 
ordered by date. There is a sharp drop in the [3 month Euribor interest rate](https://www.investopedia.com/terms/e/euribor.asp) 
aoround observation 25,000.

If we measure the efficiency of campaigns in terms of the relative
frequency of positive responses, campaigns became dramatically more 
efficient - and in that sense more successful - starting around 
observation 27,500.

While the timing suggests that the state of the economy played a role,
the improvement of the success rate might also be due to a learning effect:
the campaigns towards the end may have learned lessons from previous
campaigns and this might be an important reason why later campaigns
were more efficient/successful.

Could the improvement of campaign performance have come earlier, before
the onset of the financial crisis?

At the end of this study, a commonly highly effective machine learning 
algorithm is trained and tested both on the data before the onset of 
the financial crisis and on the data after, excluding as far as possible
information on the state of the economy. 

The model trained on the earlier data has no predictive power, which 
suggests that the data contain no useful signal. 

In contrast, the model trained on the later data has considerable 
predictive power and could have been used for optimizing campaigns. 
In fact, later campaigns appear to have been informed by such a model, 
systematically targeting subgroups and calibrating the number of calls.

Our claim is not so much that the earlier data are in fact completely 
useless, but that the later data are strikingly more useful. Dramatic
changes in the economy led to the amplification of signals in the data 
and/or to the emergence of entirely new signals. We suggest this as 
a potentially fruitful avenue for further research.

To conclude, it appears that the marked improvement of campaign performance 
was the result of a new, optimized approach that exploited changing 
attitudes in a country in crisis, and that a similar improvement would 
not have been possible before.

In particular, it is conceivable that certain identifiable subgroups 
among the clients may have become more susceptible to sales pitches 
emphasizing security and wealth protection, but this is mere speculation.
