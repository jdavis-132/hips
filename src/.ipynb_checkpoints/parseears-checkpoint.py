import pandas as pd
from statistics import mean

def parse_ear_file(mydf,ear_dict):
    for index,row in mydf.iterrows():
        myid = row["QR Code"]
        if not myid in ear_dict:
            ear_dict[myid] = {}
            for t in traitlist:
                ear_dict[myid][t] = []
            ear_dict[myid]["Kernel Mass"] = []
        for t in traitlist:
            if pd.isna(row[t]): continue
            try:
                ear_dict[myid][t].append(float(str(row[t]).replace('%','')))
            except:
                continue
        try:
            mygrainmass = float(row["Ear Weight"]) - float(row["Cob Weight"])
            if not pd.isna(mygrainmass):
                ear_dict[myid]["Kernel Mass"].append(float(row["Ear Weight"]) - float(row["Cob Weight"]))
        except:
            pass
        mycolor = str(row["Kernel Color"]).split('/')[0].replace(' ','').lower()
        if mycolor in color_dict:
            mycolor = color_dict[mycolor]
        if mycolor in set(["yellow",'orange','red']):
            ear_dict[myid]["Kernel Color"] = mycolor
        else:
            ear_dict[myid]["Kernel Color"] = ''
        if 'str' in str(row["Kernel Color"]).lower():
            ear_dict[myid]["Striping"] = 'Y'
        else:
            ear_dict[myid]["Striping"] = 'N'
    return ear_dict

df1 = pd.read_csv("../data/earphenotypesformatted.csv")
df2 = pd.read_csv("../data/earphenotypesformatted_part2.csv")
color_set = {}
ear_dict = {}
#traitlist = ["Cob Length","Ear Width","Kernel Fill Length","Cob Width","Cob Weight","100 Kernel weight","Kernel Count","Total Moisture"]
traitlist = ["Cob Length","Kernel Fill Length","Ear Width","Cob Width","Cob Weight","Kernel Count","100 Kernel weight","Total Moisture","Kernels per Row",'Kernel Row Number']

color_dict = {'y':'yellow','o':'orange','yelllow':'yellow','yelloww':'yellow','yellowredstripes':'yellow','yclear':'yellow','organge':'orange'}

ear_dict = parse_ear_file(df1,ear_dict)
ear_dict = parse_ear_file(df2,ear_dict)

header = ["BarcodeAsScanned"]
header.extend(traitlist)
header.append("KernelMass")
header.append("KernelColor")
header.append("KernelStriping")
skipped = 0
print(",".join(header))
for myid in ear_dict:
#    if len(ear_dict[myid]['Cob Length']) < 4:
#        skipped += 1
#        print(myid)
#        continue
    plist = [myid]
    for t in traitlist:
        try:
            plist.append("{0:.1f}".format(mean(ear_dict[myid][t])))
        except:
            plist.append('')
    if len(ear_dict[myid]["Kernel Mass"]) > 0:
        plist.append("{0:.1f}".format(mean(ear_dict[myid]["Kernel Mass"])))
    else:
        plist.append("")
    plist.append(ear_dict[myid]["Kernel Color"])
    plist.append(ear_dict[myid]["Striping"])
    print(",".join(plist))
#print(skipped)
#for x in reversed(sorted(list(color_set),key = lambda a:color_set[a])):
#    print('"' + x + '"',color_set[x])
