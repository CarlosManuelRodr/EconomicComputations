from ReadColumns import ReadColumns
# This function interpolates linearly to deflate the time series.

name_index = 'nasdaq'
name_infla = 'cpi'

# Number of days in each month

months = {'01' : 31,
          '02' : 29,
          '03' : 31,
          '04' : 30,
          '05' : 31,
          '06' : 30,
          '07' : 31,
          '08' : 31,
          '09' : 30,
          '10' : 31,
          '11' : 30,
          '12' : 31}


index = ReadColumns('../Data/'+name_index+'.dat')
infla = ReadColumns('../Data/'+name_infla+'.dat')

def_index = [[] for i in range(4)]

j = 0
i = 700


while (i <= len(infla[0])):
    date1 = [infla[0][i],infla[1][i],infla[2][i]]
    date2 = [infla[0][i+1],infla[1][i+1],infla[2][i+1]]
    date  = [index[0][j],index[1][j],index[2][j]]
    print date1,date,date2,
    if    (((date[0]>=date1[0])and(date[0]<date2[0]))
        and((date[1]>=date1[1])and(date[1]<date2[1]))
        and((date[2]>=date1[2])and(date[2]<date2[2]))):
        print "Yes!" #date1,date,date2
        j +=1
    elif  ((date[0]<date1[0])
        or(date[1]<date1[1])
        or(date[2]<date1[2])):
        print "No..."
        j +=1
    else:
        print "No..."
        i+=1



    
    


#N = 1000 # Size of time windows



