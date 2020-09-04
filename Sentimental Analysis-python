#!/usr/bin/env python
# coding: utf-8
#Import libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import plotly as ply
import plotly.express as px
import streamlit as st
import nltk
nltk.downloader.download('vader_lexicon')
from nltk.sentiment.vader import SentimentIntensityAnalyzer
sia = SentimentIntensityAnalyzer()
#pip install textblob
from textblob import TextBlob
import plotly.graph_objs as go
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')
nltk.download('stopwords')
from wordcloud import WordCloud
from PIL import Image

#Read the csv file
st.title("Sentiment Analysis on Amazon_Earphone_Reviews")
df= pd.read_csv(r"C:/Users/sgoradia/Desktop/Python/Amazon_Earphone reviews.csv")
df1= pd.read_csv(r"C:/Users/sgoradia/Desktop/Python/ProductInfo.csv")
st.header("Glimpse of Amazon_Earphone_Reviews dataset")
st.write(df.head())
st.write("How many rows and columns are there in the data set?")
df.shape
st.header("Products Information")
st.write(df1)
st.write("Dimensions of products information dataset:")
df1.shape

#Info about two csv files
df.info()
df1.info()

#Splitting dataset into 10 datasets based on products
df_p1=df[df.Product=="boAt Rockerz 255"]
df_p2=df[df.Product=="Flybot Wave"]
df_p3=df[df.Product=="Flybot Boom"]
df_p4=df[df.Product=="PTron Intunes"]
df_p5=df[df.Product=="Flybot Beat"]
df_p6=df[df.Product=="Samsung EO-BG950CBEIN"]
df_p7=df[df.Product=="JBL T205BT"]
df_p8=df[df.Product=="Sennheiser CX 6.0BT"]
df_p9=df[df.Product=="Skullcandy S2PGHW-174"]
df_p10=df[df.Product=="JBL T110BT"]
st.header("Reviews for first product (df_p1): boAt Rockerz 255")
st.write(df_p1.head())

#List of all 10 datassets
dblist=[df_p1,df_p2,df_p3,df_p4,df_p5,df_p6,df_p7,df_p8,df_p9,df_p10]

#Adding Polarity scores to df_p1 (reviews for boAt Rockerz 255 )
def scores(x):
    list1=[]
    x.reset_index(inplace=True)
    x.drop("index",axis=1,inplace=True)
    for i in x.ReviewBody:
        list1.append(sia.polarity_scores(i))
    x[["Negative","Neutral","Positive","Compound"]]=pd.DataFrame(list1)
for j in dblist:
    scores(j)
st.header("Adding Polarity scores to df_p1 ")
st.write(df_p1.head(10))

#Compound score for df_p1
st.header("Compound Score for df_p1")
a= px.line(y="Compound",data_frame=df_p1,width=20000, height=400)
st.plotly_chart(a)

#Adding class to df_p1
def cls(x):
    list2=[]
    for i in x["Compound"]:
        if i<0:
            list2.append("Negative")
        elif i==0:
            list2.append("Neutral")
        else:
            list2.append("Positive")
    x["Class"]=list2

for j in dblist:
    cls(j)
st.header("Adding class for df_p1")
st.write(df_p1.head(10))

st.header("How many reviews are Positive, Negative and Neutral?")
st.write(df_p1["Class"].value_counts())

Positive=df_p1[df_p1.Class=="Positive"]["Class"].count()
Negative=df_p1[df_p1.Class=="Negative"]["Class"].count()
Neutral=df_p1[df_p1.Class=="Neutral"]["Class"].count()
labels = ["Positive Comments","Negative Comments","Neutral Comments"]
values = [Positive,Negative,Neutral]
fig = go.Figure(data=[go.Pie(labels=labels, values=values,hole=0.5)])
st.header("Donut chart of Positive, Negative and Neutral comments")
st.plotly_chart(fig)

#Finding Keywords in the ReviewBody
def keys(x):
    a = [None] * len(x)
    for i in range(0,(len(x)-1)):
        list3=[]
        blob = TextBlob(x.iloc[i][1])
        for word, tag in blob.tags:
            if (tag=="JJ")| (tag=="VBN")| (tag=="NNS")| (tag=="NN"):
                list3.append(word.lemmatize())
            a[i]=list3
    x["Keywords"]=a
for j in dblist:
    keys(j)
st.header("Adding keywords to df_p1")
st.write(df_p1.head(10))

#Wordcloud for df_p1
list=[]
for i in df_p1["Keywords"] :
    list.append(i)
slist = str(list)

wordcloud = WordCloud(width=1000, height=500).generate(slist)
plt.figure(figsize=(20,20))
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.margins(x=0, y=0)
#plt.show()
st.header("WordCloud for df_p1")
st.pyplot(plt)
