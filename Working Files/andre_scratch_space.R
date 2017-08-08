train_df <- fread('train_2016_v2.csv')

sample_df <- fread('sample_submission.csv')

properties_df <- fread('properties_2016.csv')

###############################################################
# joining tain_df with properties_df
training_df <- left_join(train_df, properties_df, by='parcelid')

###############################################################
# Dropping columns
name_list <- names(training_df)

cols_drop <- c(name_list[55], name_list[5:6], name_list[9], 
               name_list[11], name_list[45], name_list[13],
               name_list[15:20], name_list[52], name_list[22],
               name_list[27:28], name_list[51], name_list[31:35],
               name_list[37:39], name_list[60], name_list[41],
               name_list[43:44], name_list[46], name_list[48:49],
               name_list[59])
cols_drop

cleanTraining <- training_df[ , !(names(training_df) %in% cols_drop)]
save(cleanTraining, file='cleanTraining.Rda')
###############################################################
# 
