[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **CV04_Merging_Tweets** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml


Name of Quantlet: 'CV04_Merging_Tweets'

Published in: 'DEDA_Class_2020SS'

Description: 'Creates the csv that will be read into create_final_df. All daily tweets will be aggregated monthly and then concatted into one csv file'

Keywords: 'Tweets, Trends, merge, aggregation, consolidation'

Author: 'Fabian Schmidt'

```

### PYTHON Code
```python

import os
import glob
import pandas as pd
import json
import csv

months = ['01']#, '02', '03', '04', '05', ]#'06', '07', '08', '09', '10', '11', '12']

for month in months:
    regex_pattern = f'../data/Tweets/by_day/*-{month}-*.csv' 
    print(f"regex pat:{regex_pattern}")
    glob_data = []
    # Concat all tweets from different days into one file
    month_filenames = [i for i in glob.glob(regex_pattern)]
    glob_data = pd.concat([pd.read_csv(f, sep=";") for f in month_filenames])
    
    #Dump the consolidated file with the format
    file_name_month = f'../data/Tweets/by_month/btc_tweets_{month}.csv' 
    glob_data.to_csv(file_name_month, index=False, encoding='utf-8')

extension = 'csv'
all_filenames = [i for i in glob.glob('../data/Tweets/by_month/*.{}'.format(extension))]

print(all_filenames)

#combine all files in the list
combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ])
#export to csv
combined_csv.to_csv("../data/Tweets/combined_tweets.csv", index=False, encoding='utf-8')

```

automatically created on 2020-08-06