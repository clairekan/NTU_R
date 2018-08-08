# -*- coding: utf-8 -*-
"""
Created on Tue Jul 31 00:53:43 2018

@author: dx788
"""

import pandas as pd
import matplotlib.pyplot as plt
import urllib.request
import json
import gzip
import sqlite3

conn = sqlite3.connect('Youbike.sqlite') #連結指定的資料庫
c = conn.cursor()#開始建立物件        
c.execute("CREATE TABLE IF NOT EXISTS Youbike_data (sno integer, sna text, sarea text, lng text, lat text, sbi integer, bemp integer, total integer, res integer);")
#站點代號(sno)、場站名稱(sna)、場站區域(sarea)、經緯度(lng/lat)、目前車輛數量(sbi)、空位數量(bemp)、總數(total)、可借百分比(res)
conn.commit()#檔案更新
url = 'http://data.taipei/youbike'
url = urllib.request.urlretrieve(url, "data.gz")
jdata = gzip.open('data.gz', 'r').read()
data = json.loads(jdata)        
for site in data["retVal"]:
    sno = data["retVal"][site]["sno"]#站點代號0
    sna = data["retVal"][site]["sna"]#場站名稱1
    sarea = data["retVal"][site]["sarea"]#場站區域2
    lng = data["retVal"][site]["lng"]#經度3
    lat = data["retVal"][site]["lat"]#緯度4
    sbi = data["retVal"][site]["sbi"]#目前車輛數量5
    bemp = data["retVal"][site]["bemp"]#空位數量6
    total = int(sbi) + int(bemp)#總數7
    try:
        res = round((int(sbi)/int(total)), 2)#可借百分比(res)8
    except:
        res = 0
    c.execute('INSERT INTO Youbike_data VALUES ({},"{}","{}","{}","{}",{},{},{},{});'.format(sno,sna,sarea,lng,lat,sbi,bemp,total,res))#插入資料
conn.commit()#檔案更新
conn.close()#檔案關閉