import json
import urllib
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import re

##use next to flip through pages

page = 1
next_page=2

columns = ['name','rating','region','price','year']

whisky_df = pd.DataFrame(columns=columns)

while next_page:
    
    whisky_url = urllib.urlopen('https://evening-citadel-85778.herokuapp.com:443/whiskey/?page='+str(page))
    
    whisky_json = json.loads(whisky_url.read())
    
    next_page = whisky_json['next']
    
    whisky_results = whisky_json['results']
   
    for whiskys in whisky_results:
        
        whisky_temp_df = pd.DataFrame(columns=columns)
        
        name = whiskys['title']
        rating = whiskys['rating']
        region = whiskys['region']
        price = whiskys['price']
        match = re.search(' [0-9][0-9] ',name)
        if match:
            year = match.group(0)
        else:
            match = re.search(' [0-9] ',name)
            if match:
                year = match.group(0)
            else:
                match = re.search(' [0-9][0-9]$',name)
                if match:
                    year = match.group(0)
                else:
                    year =''
        
        whisky_temp_df.loc[1] = [name,rating,region,price,year]
        
        whisky_df=whisky_df.append(whisky_temp_df)
        
    page+=1
    print page

all_whisky_data = whisky_df
all_whisky_data.to_csv("whisky_all.csv",index=False,encoding='utf-8')

whisky_df['year'].replace('', np.nan, inplace=True)
whisky_df.dropna(subset=['year'], inplace=True)

whisky_df['year'] = pd.to_numeric(whisky_df['year'], errors='coerce')

whisky_df.to_csv("whisky.csv",index=False)

