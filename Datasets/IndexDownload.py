from yahoo_fin import stock_info as si
from tqdm import tqdm
from datetime import date
import os

index_tickers = ['^DJI', '^IXIC', '^MXX', '^N225' ]
                
print("Downloading tickers...")

pbar = tqdm(total=len(index_tickers))

def get_intraday_data_from_ticker(ticker):
    pbar.update(1)
    return si.get_data(ticker, interval='1d', index_as_date=False)


result = list(map(lambda ticker: get_intraday_data_from_ticker(ticker), index_tickers))

if not os.path.exists('Indexes'):
    os.makedirs('Indexes')

currentDate = date.today().strftime("%Y-%m-%d")
if not os.path.exists('Indexes/' + currentDate):
    os.makedirs('Indexes/' + currentDate)
for i in range(len(index_tickers)):
    f = open('Indexes/' + currentDate + "/" + index_tickers[i] + ".csv", "w+")
    if not result[i].empty:
        f.write(result[i].drop(columns=['ticker']).to_csv(index=False))
    else:
        f.write("Failed")
    f.close()
    
print("Done!")
