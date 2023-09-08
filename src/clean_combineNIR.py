import pandas as pd
import numpy as np

df = pd.read_excel("../data/NIRData.xlsx")
dd = {}
for index, row in df.iterrows():
    myid = str(row["Sample ID"]).upper().replace(' ','')
    mymoisture = float(row["Moisture"])/100.0
    myprotein = float(row["Protein As is"])/(1-mymoisture)
    myoil = float(row["Oil As is"])/(1-mymoisture)
    myfiber = float(row["Fiber As is"])/(1-mymoisture)
    myash = float(row["Ash As is"])/(1-mymoisture)
    mystarch = float(row["Starch As is"])/(1-mymoisture)
    negvals = False
    for v in [mymoisture,myprotein,myoil,myfiber,myash,mystarch]:
        if v < 0:
            negvals = True
    if negvals: continue
    if not myid in dd: dd[myid] = {}
    for t,v in zip(["MoisturePCT","ProteinPCT","OilPCT","FiberPCT","AshPCT","StarchPCT"],[float(row['Moisture']),myprotein,myoil,myfiber,myash,mystarch]):
        if not t in dd[myid]: dd[myid][t] = []
        dd[myid][t].append(v)
    dd[myid]["RawBC"] = row["Sample ID"]

for myid in dd:
    if "SYNGENTA3111" in myid: continue
    if "NAN" in myid: continue
    idfields = myid.split('$')
    if idfields[0] in set(["MV","NORTHPLATTE","SCOTTSBLUFF","PLATTE"]):
        if idfields[0] == "NORTHPLATTE" or idfields[0] == "PLATTE":
#            mylocation = "North Platte"
            if "NO" in idfields[1]:
                myirrigation = "Rainfed"
                mylocation = "North Platte3"
            elif "Partial".upper() in idfields[1]:
                myirrigation = "Partial"
                mylocation = "North Platte2"
            elif "FULL" in idfields[1]:
                myirrigation = "Full"
                mylocation = "North Platte1"
            else:
                print(myid)
                break
            if "LOW" in idfields[1]:
                mynitrogen = "Low"
            elif "MEDIUM" in idfields[1]:
                mynitrogen = "Medium"
            elif "HIGH" in idfields[1]:
                mynitrogen = "High"
            else:
                mynitrogen = "Other"
        if idfields[0] == "MV":
            mylocation = "Missouri Valley"
            mynitrogen = "High"
            myirrigation = "Rainfed"
        if idfields[0] == "SCOTTSBLUFF":
            mylocation = "Scottsbluff"
            myirrigation = "Full"
            #This looks like a coding bug but it isn't. Treatment labels swapped.
            if "LOW" in idfields[1]:
                mynitrogen = "High"
            elif "MEDIUM" in idfields[1]:
                mynitrogen = "Medium"
            elif "HIGH" in idfields[1]:
                mynitrogen = "Low"
            else:
                mynitrogen = "Other"
        myrep = idfields[2].replace("REP",'')
        myplot = idfields[3].replace("PLOT",'')
        myrow = idfields[4].replace("ROW",'')
        myrange = idfields[5].replace("RANGE",'')
        myhybrid = idfields[6]
    else:
        mylocation = "Lincoln"
        myirrigation = "Rainfed"
        if idfields[0][0] == '4':
            mynitrogen = "Medium"
        elif idfields[0][0] == '5':
            mynitrogen = "Low"
        elif idfields[0][0] == '6':
            mynitrogen = "High"
        else:
            mynitrogen = "Other"
#        else:
#            print(idfields[0],myid)
#            continue
        myrep = ''
        myplot = idfields[0]
        myrow = idfields[1].replace("ROW",'')
        myrange  = idfields[2].replace("RANGE",'')
        myhybrid = idfields[3]

    plist = [dd[myid]["RawBC"],mylocation,mynitrogen,myirrigation,myrep,myrow,myrange,myplot,myhybrid]
    for x in ["StarchPCT","ProteinPCT","OilPCT","FiberPCT","AshPCT", "MoisturePCT"]:
        if len(dd[myid][x]) == 0:
            plist.append('')
        else:
            plist.append("{0:.2f}".format(np.median(dd[myid][x])))
    print(",".join(plist))
