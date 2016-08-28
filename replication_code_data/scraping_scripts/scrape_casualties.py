# Term Paper: State Repression in Syria
# Course: Microdynamics of State Repression
# Lecturer: Dr. Alexander De Juan
# submitted by: Johannes Willmann; (01/790064); MSc SEDA

# Script to download state repression casualties

from urllib.request import urlopen
from bs4 import BeautifulSoup
import codecs

f = codecs.open('syriawar2.csv', 'w', 'utf-8')
f.write("Name" + "; " + "Status" + "; " + "Sex" + "; " + "Province" "; "
        + "Area" + "; " + "Date of death" + "; " + "Cause of death;" + "\n")



for x in range(1375, 988, -1):
    syria = "file" + "\t" + str(x)
    print("fetching data ... " + syria)
    url = 'http://www.vdc-sy.info/index.php/en/martyrs/' + str(x) + '/c29ydGJ5PWEua2lsbGVkX2RhdGV8c29ydGRpcj1ERVNDfGFwcHJvdmVkPXZpc2libGV8ZXh0cmFkaXNwbGF5PTB8'
    page = urlopen(url)
    soup = BeautifulSoup(page)

    rows = soup.findAll('tr')

    i = 0
    for row in rows[6:]:
        if i % 2 == 0:
            tds = row.find_all('td')
            for j in tds:
                f.write("%s; " %
                    (str(j.contents[0]).strip()))
            f.write("\n")
        i += 1

f.close()