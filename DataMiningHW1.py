#!/usr/bin/env python
# coding: utf-8

# In[15]:


import pandas as pd
import numpy as np 
import seaborn as sns
import matplotlib.pyplot as plt
import sklearn
from mlxtend.frequent_patterns import apriori
from mlxtend.frequent_patterns import association_rules
import mlxtend as ml


# In[5]:


#get the file and take a look
rest = pd.read_csv("/Users/jacksonperry/Desktop/Fall 2/restaurantData.csv")
rest.head(8)


# In[7]:


#look a the most common items on the list
sns.countplot(x = 'order', data = rest, order = rest['order'].value_counts().iloc[:10].index)
plt.xticks(rotation=90)


# In[8]:


#encode/group the orders by orderNumber
df = rest.groupby(['orderNumber','order']).size().reset_index(name='count')
basket = (df.groupby(['orderNumber', 'order'])['count']
          .sum().unstack().reset_index().fillna(0)
          .set_index('orderNumber'))

def encode_units(x):
    if x <= 0:
        return 0
    if x >= 1:
        return 1
basket_sets = basket.applymap(encode_units)


# In[20]:


#do the association analysis
frequent_itemsets = apriori(basket_sets, min_support=0.1, use_colnames=True)
rules = association_rules(frequent_itemsets, metric="lift")
rules.sort_values('confidence', ascending = False, inplace = True)
rules.head(10)


# In[ ]:





# In[ ]:




