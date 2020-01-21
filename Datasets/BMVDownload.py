from yahoo_fin import stock_info as si
from tqdm import tqdm
from datetime import date
import os

ipc_tickers = [ 'AC.MX','ALPEKA.MX','ALSEA.MX','AMXL.MX','ASURB.MX','BBAJIOO.MX',
                'BIMBOA.MX','BOLSAA.MX','BSMXB.MX','CEMEXCPO.MX','CUERVO.MX','GAPB.MX',
                'GCARSOA1.MX','GCC.MX','GENTERA.MX','GFNORTEO.MX','GMEXICOB.MX',
                'GRUMAB.MX','IENOVA.MX','KIMBERA.MX','KOF','LABB.MX','LIVEPOLC-1.MX',
                'MEGACPO.MX','ORBIA.MX','OMAB.MX','PE&OLES.MX','PINFRA.MX','RA.MX',
                'TLEVISACPO.MX']
                
print("Downloading tickers...")

pbar = tqdm(total=len(ipc_tickers))

def get_intraday_data_from_ticker(ticker):
    pbar.update(1)
    return si.get_data(ticker, interval='1m', index_as_date=False)


result = list(map(lambda ticker: get_intraday_data_from_ticker(ticker), ipc_tickers))

if not os.path.exists('BMV'):
    os.makedirs('BMV')

currentDate = date.today().strftime("%d-%m-%Y")
if not os.path.exists('BMV/' + currentDate):
    os.makedirs('BMV/' + currentDate)
for i in range(len(ipc_tickers)):
    f = open('BMV/' + currentDate + "/" + ipc_tickers[i] + ".csv", "w+")
    if not result[i].empty:
        f.write(result[i].drop(columns=['ticker']).to_csv(index=False))
    else:
        f.write("Failed")
    f.close()
    
print("Done!")
