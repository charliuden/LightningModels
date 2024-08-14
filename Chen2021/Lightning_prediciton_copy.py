import pandas as pd
import numpy as np

#montly means for ERA5 data and Vaisala Lightning data
df = pd.read_csv("/raid/cuden/data/era5_vaisalaLightning_montlySummaries_NEclip.csv")
#print(df)

cape = df["cape_monthly_mean"]
precip = df["mtpr_monthly_mean"]
cxp = cape*precip
lr = df["mean_strike_rate"]

##--- Power Law ---
# read parameters
vtmp = pd.read_csv('/raid/cuden/LightningModels/Chen2021/FR.vs.CxP_data/FR.vs.CxP_pl.csv',index_col=0)
a_power,b_power = vtmp['0']['a'],vtmp['0']['b']
pl =  a_power*cxp**b_power

print(pl)

#--- power law (linear optimization) ---
vtmp = pd.read_csv('/raid/cuden/LightningModels/Chen2021/FR.vs.CxP_data/FR.vs.CxP_pl_op.csv',index_col=0)
a_power_op,b_power_op = vtmp['0']['a'],vtmp['0']['b']
pl_op =  a_power_op*cxp**b_power_op

#--- scale ---
vtmp = pd.read_csv('/raid/cuden/LightningModels/Chen2021/FR.vs.CxP_data/FR.vs.CxP_sc.csv',index_col=0)
a_scale = vtmp['0']['a']
sc = cxp*a_scale

#--- linear ---
vtmp = pd.read_csv('/raid/cuden/LightningModels/Chen2021/FR.vs.CxP_data/FR.vs.CxP_li.csv',index_col=0)
a_linear,b_linear = vtmp['0']['a'],vtmp['0']['b']
li = np.clip(a_linear*cxp+b_linear,0,None)


#--- linear 2 ---
vtmp = pd.read_csv('/raid/cuden/LightningModels/Chen2021/FR.vs.CxP_data/FR.vs.CxP_li2.csv',index_col=0)
a_linear2,b_linear2 = vtmp['0']['a'],vtmp['0']['b']        
li2 =  np.clip(a_linear2*cxp+b_linear2,0,None)

#--- non parametric ---
#vtmp = pd.read_csv('/raid/cuden/LightningModels/Chen2021/FR.vs.CxP_data/FR.vs.CxP_np.csv',index_col=0)
#binedge,bincenter,Ybinmn,Ybinstd,oobpara = vtmp.loc['binedge'].values,vtmp.loc['bincenter'].values,vtmp.loc['Ybinmn'].values,vtmp.loc['Ybinstd'].values,vtmp.loc['oobpara'].values

#xbin,ybin=bincenter[:-1],Ybinmn[:-1]
#nopa = np.zeros((180,360))

#for ix in range(180):
    #for iy in range(360):
        #vx = cxp
        #if vx<xbin[-1]:
            #nopa[ix,iy] = np.interp(vx,xbin,ybin)
        #else:
            #nopa[ix,iy] = oobpara[0] + oobpara[1]*vx 
                     
#print(nopa)

 # the final projection is using modeled FRfuture/FRhist to scale observed FR - but we aren't modeling lightning under future climate scenarios, so this step can't be done...

#save predicted values
pred = pd.DataFrame({'pl': pl, 'pl_op': pl_op, 'sc': sc, 'li': li, 'li2': li2})

print(pred)

pred.to_csv("/raid/cuden/data/Chen2021_NEpredictions.csv")


