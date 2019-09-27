# Routine format_inflation
# Written by Hector Olivares
# This script reads the data series of the American Consumer Price Index
# and the Mexican Indice Nacional de Precios al Consumidor and writes them in a
# format that will be later easibly readable for writing other time series in constant money.

import operator # For sorting

# Lists containing: Year, Month, Day, IndexValue
cpi = []
inpc = []

# Dictionary of the months, for the INPC
months = {'Ene' : 1,
          'Feb' : 2,
          'Mar' : 3,
          'Abr' : 4,
          'May' : 5,
          'Jun' : 6,
          'Jul' : 7,
          'Ago' : 8,
          'Sep' : 9,
          'Oct' : 10,
          'Nov' : 11,
          'Dic' : 12}

##### CPI file
CPI_in_file = open("../Data/ConsumerPriceIndex","r")
CPI_in = CPI_in_file.readlines()
CPI_in_file.close()

for year in range(1,len(CPI_in)):
    row = CPI_in[year].split()
    for month in range(1,min(len(row),13)):
#        print row[0], "%02d" % month,'01',row[month]
        cpi.append([int(row[0]),month,1,float(row[month])])

cpi.sort(key = operator.itemgetter(0,1))

CPI_out_file = open("../Data/cpi.dat","w")

for year in range(len(cpi)):
    CPI_out_file.write('%d     %02d     %02d     %.3f\n' %
                       (cpi[year][0],cpi[year][1],cpi[year][2],cpi[year][3]))

CPI_out_file.close()

##### INPC file
INPC_in_file = open("../Data/INPC_list","r")
INPC_in = INPC_in_file.readlines()
INPC_in_file.close()

for date in range(1,len(INPC_in)):
    row = INPC_in[date].split()
    if len(row) > 0:    
        day = 1 if row[0]=='1Q' else 16
        month = months[row[1]]
        year = int(row[2])
        index = float(row[3])
        inpc.append([year,month,day,index])

INPC_out_file = open("../Data/inpc.dat","w")

for year in range(len(inpc)):
    INPC_out_file.write('%d     %02d     %02d     %.3f\n' %
                       (inpc[year][0],inpc[year][1],inpc[year][2],inpc[year][3]))

INPC_out_file.close()


