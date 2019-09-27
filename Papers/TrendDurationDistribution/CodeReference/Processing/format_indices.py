# Routine format_indices
# Written by Hector Olivares, 07.04.2016
# This script reads the data from csv time series
# of the DJIA, Nasdaq and IPC indexes into an array with a format similar
# to that used for inflation in the script format_inflation.py

import operator # for sorting
import re       # for splitting
# Lists containing: Year, Month, Day, IndexValue

###------------ DJI --------------
#
#dji = []
#
#DJI_in_file = open("../Data/BCB-DJIA_14_07_1896-17_03_2016.csv","r")
#DJI_in = DJI_in_file.readlines()
#DJI_in_file.close()
#
#for line in DJI_in:
#    row = re.split('-|,',line)
#    if len(row) > 3:    
#        day = int(row[2])
#        month = int(row[1])
#        year = int(row[0])
#        index = float(row[3])
#        dji.append([year,month,day,index])
#
#DJI_out_file = open("../Data/dji.dat","w")
#
#
#dji.sort(key = operator.itemgetter(0,1,2))
#
#
#for year in range(len(dji)):
#    DJI_out_file.write('%d     %02d     %02d     %.3f\n' %
#                       (dji[year][0],dji[year][1],dji[year][2],dji[year][3]))
#
#DJI_out_file.close()
#
#
####-------------- NASDAQ ----------------
#
#nasdaq = []
#
##NASDAQ_in_file = open("../Data/Nasdaq_05_02_1971-17_03_2016.csv","r")
#NASDAQ_in_file = open("../Data/IPC_08_11_1991-17_03_2016.csv","r")
#NASDAQ_in = NASDAQ_in_file.readlines()
#NASDAQ_in_file.close()
#
#for i in range(1,len(NASDAQ_in)):
#    row = re.split('-|,',NASDAQ_in[i])
#    if len(row) > 3:    
#        day = int(row[2])
#        month = int(row[1])
#        year = int(row[0])
#        index = float(row[6])
#        nasdaq.append([year,month,day,index])
#
##NASDAQ_out_file = open("../Data/nasdaq.dat","w")
#NASDAQ_out_file = open("../Data/ipc2.dat","w")
#
#nasdaq.sort(key = operator.itemgetter(0,1,2))
#
#
#for year in range(len(nasdaq)):
#    NASDAQ_out_file.write('%d     %02d     %02d     %.3f\n' %
#                       (nasdaq[year][0],nasdaq[year][1],nasdaq[year][2],nasdaq[year][3]))
#
#NASDAQ_out_file.close()
#

###---------------- IPC ------------------

ipc = []

IPC_in_file = open("../Data/IPC_08_11_1991-17_03_2016.csv","r")
IPC_in = IPC_in_file.readlines()
IPC_in_file.close()

for line in IPC_in:
    row = re.split('-|,',line)
    if len(row) > 8:    
        day = int(row[2])
        month = int(row[1])
        year = int(row[0])
        index = float(row[6])
        ipc.append([year,month,day,index])

IPC_out_file = open("../Data/ipc2.dat","w")


ipc.sort(key = operator.itemgetter(0,1,2))


for year in range(len(ipc)):
    IPC_out_file.write('%d     %02d     %02d     %.3f\n' %
                       (ipc[year][0],ipc[year][1],ipc[year][2],ipc[year][3]))

IPC_out_file.close()

