# zillow_project

8/10, 8:00 PM - Added scratch code and new dataframe, cleanTraining2. This dataframe adds new features (month, room count), and scales the long/lat variables. It also imputes some variables to binary that had not been done, and changes the type of several variables that should be factors or numeric.
A few outstanding questions: 
1) What should we do with zipcode? Can't leave it as a factor because there are too many levels. Maybe we can join other data based on latitude and longitude. 
2) What about lat/long? Right now they are numeric, does this make sense?
3) Change imputation of bathroomcount to mode so it can be replicated. 

#RECOMMEND MERGING THIS CODE WITH MASTER CODE AND RE-RUNNING TO OBTAIN A FINAL CLEAN DATA FRAME. RIGHT NOW THE CODE IS NOT WELL-ORGANIZED.
