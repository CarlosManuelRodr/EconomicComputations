import numpy as np
import re

def ReadColumns(name):
    f = open(name,"r")
    lines = f.readlines()
    f.close()
    nC = len(lines[10].split())
    nR = len(lines)
    print name,"\tNo. of Columns: ",nC
    print name,"\tNo. of Rows: ",nR
    lists = [[] for i in range(nC)]
    arrays = []
    for line in lines:
        p = line.split()
#        print p
        # print nC,p
        for i in range(nC):
            if len(p)== nC:
                try:
#                    print p[i],
                    lists[i].append(float(p[i]))
                except ValueError,IndexError:
                    lists[i].append(None)
        # print
    for i in range(nC):
        arrays.append(np.array(lists[i]))
    return arrays
        
        

# Col = ReadColumns('3DFinal/Lightcurve_AMRtol0p1NEW_i0p1.dat')


