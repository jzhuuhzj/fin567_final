import pandas as pd
import datetime
import tqdm
import math

loc = '~/Desktop/fin567_project/'

stock_price = pd.read_csv('{loc}stock_data.csv'.format(loc=loc))
stock_price['date'] = pd.to_datetime(stock_price['date'])

loc='~/Desktop/fin567_project/option_data/'
DJI = pd.read_csv('{loc}DJI.csv'.format(loc=loc))
MMM = pd.read_csv('{loc}MMM.csv'.format(loc=loc))
AAPL = pd.read_csv('{loc}AAPL.csv'.format(loc=loc))
AXP = pd.read_csv('{loc}AXP.csv'.format(loc=loc))
BA = pd.read_csv('{loc}BA.csv'.format(loc=loc))
CAT = pd.read_csv('{loc}CAT.csv'.format(loc=loc))
CVX = pd.read_csv('{loc}CVX.csv'.format(loc=loc))
CSCO = pd.read_csv('{loc}CSCO.csv'.format(loc=loc))
KO = pd.read_csv('{loc}KO.csv'.format(loc=loc))
XOM = pd.read_csv('{loc}XOM.csv'.format(loc=loc))
GE = pd.read_csv('{loc}GE.csv'.format(loc=loc))
GS = pd.read_csv('{loc}GS.csv'.format(loc=loc))
HD = pd.read_csv('{loc}HD.csv'.format(loc=loc))
INTC = pd.read_csv('{loc}INTC.csv'.format(loc=loc))
IBM = pd.read_csv('{loc}IBM.csv'.format(loc=loc))
JNJ = pd.read_csv('{loc}JNJ.csv'.format(loc=loc))
JPM = pd.read_csv('{loc}JPM.csv'.format(loc=loc))
MCD = pd.read_csv('{loc}MCD.csv'.format(loc=loc))
MRK = pd.read_csv('{loc}MRK.csv'.format(loc=loc))
MSFT = pd.read_csv('{loc}MSFT.csv'.format(loc=loc))
NKE = pd.read_csv('{loc}NKE.csv'.format(loc=loc))
PFE = pd.read_csv('{loc}PFE.csv'.format(loc=loc))
PG = pd.read_csv('{loc}PG.csv'.format(loc=loc))
TRV = pd.read_csv('{loc}TRV.csv'.format(loc=loc))
UNH = pd.read_csv('{loc}UNH.csv'.format(loc=loc))
UTX = pd.read_csv('{loc}UTX.csv'.format(loc=loc))
VZ = pd.read_csv('{loc}VZ.csv'.format(loc=loc))
V = pd.read_csv('{loc}V.csv'.format(loc=loc))
WMT = pd.read_csv('{loc}WMT.csv'.format(loc=loc))
DIS = pd.read_csv('{loc}DIS.csv'.format(loc=loc))
DWDP = pd.read_csv('{loc}DWDP.csv'.format(loc=loc))

def get_vol(ticker, name):
    
    ticker['date'] = pd.to_datetime(ticker['date'])
    ticker['exdate'] = pd.to_datetime(ticker['exdate'])
    ticker['strike_price'] = ticker['strike_price']/1000
    stock_price[name+'_call.vol'] = 999.999999
    stock_price[name+'_put.vol'] = 999.999999
    for i in range(0,len(stock_price[name+'.Adjusted'])):
        date = stock_price['date'][i]
        lalala = ticker[(ticker.date == date)&(ticker.exdate == '2017-12-15')]
        lalala_call = lalala[lalala['cp_flag'] == 'C']
        lalala_put = lalala[lalala['cp_flag'] == 'P']
        lalala_call = lalala_call.reset_index()
        lalala_put = lalala_put.reset_index()
        difference_call = 99999.999999
        difference_put = 99999.999999
        for j in range(0,len(lalala_call['strike_price'])):
            diff_call = abs(stock_price[name+'.Adjusted'][i] - lalala_call['strike_price'][j])
            if diff_call < difference_call:
                difference_call = diff_call
                stock_price[name+'_call.vol'][i] = lalala_call['impl_volatility'][j]
        for k in range(0,len(lalala_put['strike_price'])):
            diff_put = abs(stock_price[name+'.Adjusted'][i] - lalala_put['strike_price'][k])
            if diff_put < difference_put:
                difference_put = diff_put
                stock_price[name+'_put.vol'][i] = lalala_put['impl_volatility'][k]
    
    stock_price[name+'.vol'] = 99.999999
    for c in range(0, len(stock_price['date'])):
        if math.isnan(stock_price[name + '_call.vol'][c]):
            stock_price[name +'_call.vol'][c] = stock_price[name +'_put.vol'][c]
        if math.isnan(stock_price[name +'_put.vol'][c]):
            stock_price[name +'_put.vol'][c] = stock_price[name +'_call.vol'][c]
    for d in range(0, len(stock_price['date'])):
        if math.isnan(stock_price[name +'_call.vol'][d]):
            if d != 0:
                stock_price[name +'_call.vol'][d] = stock_price[name +'_call.vol'][d-1]
            else:
                stock_price[name +'_call.vol'][d] = stock_price[name +'_call.vol'][d+1]
        if math.isnan(stock_price[name +'_put.vol'][d]):
            if d != 0:
                stock_price[name +'_put.vol'][d] = stock_price[name +'_put.vol'][d-1]
            else:
                stock_price[name +'_put.vol'][d] = stock_price[name +'_put.vol'][d+1]           

    for e in range(len(stock_price['date'])-1, -1, -1):
        if stock_price[name + '_call.vol'][e] == 999.999999:
            if stock_price[name + '_put.vol'][e] == 999.999999:
                stock_price[name +'_call.vol'][e] = stock_price[name +'_call.vol'][e+1]
                stock_price[name + '_put.vol'][e] == 666.666666
            else:
                stock_price[name +'_call.vol'][e] = stock_price[name +'_put.vol'][e]
        if stock_price[name +'_put.vol'][e] == 666.666666:
            stock_price[name +'_put.vol'][e] = stock_price[name +'_put.vol'][e+1]
        if stock_price[name +'_put.vol'][e] == 999.999999:
            stock_price[name +'_put.vol'][e] = stock_price[name +'_call.vol'][e]        
    
        stock_price[name +'.vol'][e] = (stock_price[name +'_call.vol'][e] + stock_price[name +'_put.vol'][e])/2

def get_vol(ticker, name):
    
    ticker['date'] = pd.to_datetime(ticker['date'])
    ticker['exdate'] = pd.to_datetime(ticker['exdate'])
    ticker['strike_price'] = ticker['strike_price']/1000
    stock_price[name+'_call.vol'] = 999.999999
    stock_price[name+'_put.vol'] = 999.999999
    stock_price[name+'_call.vega'] = 999.999999
    stock_price[name+'_call.theta'] = 999.99999
    stock_price[name+'_call.strike'] = 999.99999
    stock_price[name+'_call.bid'] = 999.99
    stock_price[name+'_call.ask'] = 999.99
    stock_price[name+'_call.datadate'] = 'lalala'
    stock_price[name+'_put.vega'] = 999.999999
    stock_price[name+'_put.theta'] = 999.99999
    stock_price[name+'_put.strike'] = 999.99999
    stock_price[name+'_put.bid'] = 999.99
    stock_price[name+'_put.ask'] = 999.99
    stock_price[name+'_put.datadate'] = 'lalala'
    
    for i in range(0,len(stock_price[name+'.Adjusted'])):
        date = stock_price['date'][i]
        lalala = ticker[(ticker.date == date)&(ticker.exdate == '2017-12-15')]
        lalala_call = lalala[lalala['cp_flag'] == 'C']
        lalala_put = lalala[lalala['cp_flag'] == 'P']
        lalala_call = lalala_call.reset_index()
        lalala_put = lalala_put.reset_index()
        difference_call = 99999.999999
        difference_put = 99999.999999
        stock_price[name+'_call.datadate'][i] = date
        stock_price[name+'_put.datadate'][i] = date
        
        for j in range(0,len(lalala_call['strike_price'])):
            diff_call = abs(stock_price[name+'.Adjusted'][i] - lalala_call['strike_price'][j])
            if diff_call < difference_call:
                difference_call = diff_call
                stock_price[name+'_call.vol'][i] = lalala_call['impl_volatility'][j]
                stock_price[name+'_call.vega'][i] = lalala_call['vega'][j]
                stock_price[name+'_call.theta'][i] = lalala_call['theta'][j]
                stock_price[name+'_call.strike'][i] = lalala_call['strike_price'][j]
                stock_price[name+'_call.bid'][i] = lalala_call['best_bid'][j]
                stock_price[name+'_call.ask'][i] = lalala_call['best_offer'][j]
        
        for k in range(0,len(lalala_put['strike_price'])):
            diff_put = abs(stock_price[name+'.Adjusted'][i] - lalala_put['strike_price'][k])
            if diff_put < difference_put:
                difference_put = diff_put
                stock_price[name+'_put.vol'][i] = lalala_put['impl_volatility'][k]
                stock_price[name+'_put.vega'][i] = lalala_put['vega'][k]
                stock_price[name+'_put.theta'][i] = lalala_put['theta'][k]
                stock_price[name+'_put.strike'][i] = lalala_put['strike_price'][k]
                stock_price[name+'_put.bid'][i] = lalala_put['best_bid'][k]
                stock_price[name+'_put.ask'][i] = lalala_put['best_offer'][k]
    
    stock_price[name+'.vol'] = 99.999999
    for c in range(0, len(stock_price['date'])):
        if math.isnan(stock_price[name + '_call.vol'][c]):
            stock_price[name +'_call.vol'][c] = stock_price[name +'_put.vol'][c]
            stock_price[name +'_call.vega'][c] = stock_price[name +'_put.vega'][c]
            stock_price[name +'_call.theta'][c] = stock_price[name +'_put.theta'][c]
            stock_price[name+'_call.strike'][c] = stock_price[name+'_put.strike'][c]
            stock_price[name +'_call.bid'][c] = stock_price[name +'_put.bid'][c]
            stock_price[name +'_call.ask'][c] = stock_price[name +'_put.ask'][c]
        if math.isnan(stock_price[name +'_put.vol'][c]):
            stock_price[name +'_put.vol'][c] = stock_price[name +'_call.vol'][c]
            stock_price[name +'_put.vega'][c] = stock_price[name +'_call.vega'][c]
            stock_price[name +'_put.theta'][c] = stock_price[name +'_call.theta'][c]
            stock_price[name+'_put.strike'][c] = stock_price[name+'_call.strike'][c]
            stock_price[name +'_put.bid'][c] = stock_price[name +'_call.bid'][c]
            stock_price[name +'_put.ask'][c] = stock_price[name +'_call.ask'][c]
    
    for d in range(0, len(stock_price['date'])):
        if math.isnan(stock_price[name +'_call.vol'][d]):
            if d != 0:
                stock_price[name +'_call.vol'][d] = stock_price[name +'_call.vol'][d-1]
                stock_price[name +'_call.vega'][d] = stock_price[name +'_call.vega'][d-1]
                stock_price[name +'_call.theta'][d] = stock_price[name +'_call.theta'][d-1]
                stock_price[name+'_call.strike'][d] = stock_price[name+'_call.strike'][d-1]
                stock_price[name +'_call.bid'][d] = stock_price[name +'_call.bid'][d-1]
                stock_price[name +'_call.ask'][d] = stock_price[name +'_call.ask'][d-1]
                stock_price[name +'_call.datadate'][d] = stock_price[name +'_call.datadate'][d-1]
            else:
                stock_price[name +'_call.vol'][d] = stock_price[name +'_call.vol'][d+1]
                stock_price[name +'_call.vega'][d] = stock_price[name +'_call.vega'][d+1]
                stock_price[name +'_call.theta'][d] = stock_price[name +'_call.theta'][d+1]
                stock_price[name+'_call.strike'][d] = stock_price[name+'_call.strike'][d+1]
                stock_price[name +'_call.bid'][d] = stock_price[name +'_call.bid'][d+1]
                stock_price[name +'_call.ask'][d] = stock_price[name +'_call.ask'][d+1]
                stock_price[name +'_call.datadate'][d] = stock_price[name +'_call.datadate'][d+1]
        if math.isnan(stock_price[name +'_put.vol'][d]):
            if d != 0:
                stock_price[name +'_put.vol'][d] = stock_price[name +'_put.vol'][d-1]
                stock_price[name +'_put.vega'][d] = stock_price[name +'_put.vega'][d-1]
                stock_price[name +'_put.theta'][d] = stock_price[name +'_put.theta'][d-1]
                stock_price[name+'_put.strike'][d] = stock_price[name+'_put.strike'][d-1]
                stock_price[name +'_put.bid'][d] = stock_price[name +'_put.bid'][d-1]
                stock_price[name +'_put.ask'][d] = stock_price[name +'_put.ask'][d-1]
                stock_price[name +'_put.datadate'][d] = stock_price[name +'_put.datadate'][d-1]
            else:
                stock_price[name +'_put.vol'][d] = stock_price[name +'_put.vol'][d+1]
                stock_price[name +'_put.vega'][d] = stock_price[name +'_put.vega'][d+1]
                stock_price[name +'_put.theta'][d] = stock_price[name +'_put.theta'][d+1]
                stock_price[name+'_put.strike'][d] = stock_price[name+'_put.strike'][d+1]
                stock_price[name +'_put.bid'][d] = stock_price[name +'_put.bid'][d+1]
                stock_price[name +'_put.ask'][d] = stock_price[name +'_put.ask'][d+1]
                stock_price[name +'_put.datadate'][d] = stock_price[name +'_put.datadate'][d+1]

    for e in range(len(stock_price['date'])-1, -1, -1):
        if stock_price[name + '_call.vol'][e] == 999.999999:
            if stock_price[name + '_put.vol'][e] == 999.999999:
                stock_price[name +'_call.vol'][e] = stock_price[name +'_call.vol'][e+1]
                stock_price[name +'_call.vega'][e] = stock_price[name +'_call.vega'][e+1]
                stock_price[name +'_call.theta'][e] = stock_price[name +'_call.theta'][e+1]
                stock_price[name+'_call.strike'][e] = stock_price[name+'_call.strike'][e+1]
                stock_price[name +'_call.bid'][e] = stock_price[name +'_call.bid'][e+1]
                stock_price[name +'_call.ask'][e] = stock_price[name +'_call.ask'][e+1]
                stock_price[name +'_call.datadate'][e] = stock_price[name +'_call.datadate'][e+1]
                stock_price[name + '_put.vol'][e] == 666.666666
            else:
                stock_price[name +'_call.vol'][e] = stock_price[name +'_put.vol'][e]
                stock_price[name +'_call.vega'][e] = stock_price[name +'_put.vega'][e]
                stock_price[name +'_call.theta'][e] = stock_price[name +'_put.theta'][e]
                stock_price[name+'_call.strike'][e] = stock_price[name+'_put.strike'][e]
                stock_price[name +'_call.bid'][e] = stock_price[name +'_put.bid'][e]
                stock_price[name +'_call.ask'][e] = stock_price[name +'_put.ask'][e]
        if stock_price[name +'_put.vol'][e] == 666.666666:
            stock_price[name +'_put.vol'][e] = stock_price[name +'_put.vol'][e+1]
            stock_price[name +'_put.vega'][e] = stock_price[name +'_put.vega'][e+1] 
            stock_price[name +'_put.theta'][e] = stock_price[name +'_put.theta'][e+1]
            stock_price[name+'_put.strike'][e] = stock_price[name+'_put.strike'][e+1]
            stock_price[name +'_put.bid'][e] = stock_price[name +'_put.bid'][e+1] 
            stock_price[name +'_put.ask'][e] = stock_price[name +'_put.ask'][e+1] 
            stock_price[name +'_put.datadate'][e] = stock_price[name +'_put.datadate'][e+1] 
        if stock_price[name +'_put.vol'][e] == 999.999999:
            stock_price[name +'_put.vol'][e] = stock_price[name +'_call.vol'][e]
            stock_price[name +'_put.vega'][e] = stock_price[name +'_call.vega'][e]
            stock_price[name +'_put.theta'][e] = stock_price[name +'_call.theta'][e]
            stock_price[name+'_put.strike'][e] = stock_price[name+'_call.strike'][e]
            stock_price[name +'_put.bid'][e] = stock_price[name +'_call.bid'][e]
            stock_price[name +'_put.ask'][e] = stock_price[name +'_call.ask'][e]
    
        stock_price[name +'.vol'][e] = (stock_price[name +'_call.vol'][e] + stock_price[name +'_put.vol'][e])/2
    
    stock_price[name +'_call.datadate'] = pd.to_datetime(stock_price[name +'_call.datadate'])
    stock_price[name +'_put.datadate'] = pd.to_datetime(stock_price[name +'_put.datadate'])

get_vol(DJI,'DJI')
get_vol(MMM,'MMM')
get_vol(AAPL,'AAPL')
get_vol(AXP,'AXP')
get_vol(BA,'BA')
get_vol(CAT,'CAT')
get_vol(CVX,'CVX')
get_vol(CSCO,'CSCO')
get_vol(KO,'KO')
get_vol(XOM,'XOM')
get_vol(GE,'GE')
get_vol(GS,'GS')
get_vol(HD,'HD')
get_vol(INTC,'INTC')
get_vol(IBM,'IBM')
get_vol(JNJ,'JNJ')
get_vol(JPM,'JPM')
get_vol(MCD,'MCD')
get_vol(MRK,'MRK')
get_vol(MSFT,'MSFT')
get_vol(NKE,'NKE')
get_vol(PFE,'PFE')
get_vol(PG,'PG')
get_vol(TRV,'TRV')
get_vol(UNH,'UNH')
get_vol(UTX,'UTX')
get_vol(VZ,'VZ')
get_vol(V,'V')
get_vol(WMT,'WMT')
get_vol(DIS,'DIS')
get_vol(DWDP,'DWDP')

stock_price

stock_price.to_csv("new_stock_price_and_volatility.csv", index=False, encoding='utf-8')