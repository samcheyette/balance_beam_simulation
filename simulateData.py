import pandas as pd
import numpy as np
from numpy.random import normal
from numpy import exp, log
from scipy.stats import beta, multinomial
from random import shuffle



def inv_logit(x):
    return (exp(x)/(1+exp(x)))


def logit(p):
    return log(p) - log(1-p)



# Load the dataset
file_path = '../pilot_dat_cleaned.csv'
data = pd.read_csv(file_path)

outer_dict = {}

for index, row in data.iterrows():

    trialName = row["CleanTrialName"]
    inner_dict = {
        "TrialName": row["CleanTrialName"],
        'NormCorrectBeamResponse': row['NormCorrectBeamResponse'],
        'WeightSide': row['WeightSide'],
        'ModLeft': row['ModLeft'],
        'ModBal': row['ModBal'],
        'ModRight': row['ModRight'],
        'ItemID': row['ItemID']

    }


    outer_dict[row['CleanTrialName']] = inner_dict



CWTrials, CDTrials, CBTrials = [],[],[]

for trialName in outer_dict:
    if "CW" in trialName:
        CWTrials.append(outer_dict[trialName])
    elif "CD" in trialName:
        CDTrials.append(outer_dict[trialName])
    else:
        CBTrials.append(outer_dict[trialName])




condition="none"
for icpt in [0]:
    # for condition in ["blocked", "mixed"]:

    #     if condition == "blocked":

    #         useWeightGrp_icpt =-0.4
    #         weightVar_icpt = 3.5

    #         useWeightGrp_lowmem = 0.5
    #         weightVar_lowmem = 1.75

    #         useWeightGrp_highmem = 1.6
    #         weightVar_highmem = 2.6

    #         randomResponseGrp = 0.16
    #         randomScale = 3.1
    #     else:


    useWeightGrp_icpt = -0.4 + icpt
    weightVar_icpt = 3.5

    useWeightGrp_lowmem = 0
    weightVar_lowmem = 1.75

    useWeightGrp_highmem = 0
    weightVar_highmem =2.6

    randomResponseGrp = 0.16
    randomScale = 3.1

    # if icpt == "same_icpt":
    #     useWeightGrp_icpt =-0.4
    #     weightVar_icpt = 3.5

    #     useWeightGrp_lowmem = -0.1
    #     weightVar_lowmem = 0.3

    #     useWeightGrp_highmem = -0.2
    #     weightVar_highmem = 0.6

    #     randomResponseGrp = 0.18
    #     randomScale = 1.2
    # else:
    #     useWeightGrp_icpt = 1.2
    #     weightVar_icpt = 3.5

    #     useWeightGrp_lowmem = 0
    #     weightVar_lowmem = 1.75

    #     useWeightGrp_highmem = 0
    #     weightVar_highmem =2.6

    #     randomResponseGrp = 0.16
    #     randomScale = 3.1


    o = "Experiment,Subject,trueUseWeightGrp_icpt,trueWeightVar_icpt,trueUseWeightGrp_lowmem,trueWeightVar_lowmem,trueUseWeightGrp_highmem,trueWeightVar_highmem,"
    o += "trueRandomResponseGrp,trueRandomScale,trueUseWeightSubj_icpt,trueUseWeightSubj_lowmem,trueUseWeightSubj_highmem,trueRandomResponseSubj,"
    o += "TrialOrder,ItemID,CleanTrialName,TrialType,CogLoad,NormCorrectBeamResponse,WeightSide,ModLeft,ModBal,ModRight,NormBeamResponse\n"

    for subj in range(1,151):

        useWeightSubj_icpt = normal(useWeightGrp_icpt, weightVar_icpt)
        useWeightSubj_lowmem = normal(useWeightGrp_lowmem, weightVar_lowmem)
        useWeightSubj_highmem = normal(useWeightGrp_highmem, weightVar_highmem) 

        randomResponseSubj = max(beta.rvs(randomScale * randomResponseGrp, randomScale * (1-randomResponseGrp)),0.001)


        shuffle(CWTrials)
        shuffle(CDTrials)
        shuffle(CBTrials)
        c, TrialId = 0, 1

        for k in range(16):

            for cogLoad in [0,1,2]:
                pUseWeight = inv_logit(useWeightSubj_icpt + useWeightSubj_lowmem * (cogLoad == 1) + useWeightSubj_highmem * (cogLoad == 2))
                for trialType in ["CW","CD","CB"]:
                    if trialType == "CW":
                        trialInfoDct = CWTrials[c]
                    elif trialType == "CD": 
                        trialInfoDct = CDTrials[c]
                    else:
                        trialInfoDct = CBTrials[c]

                    TrialName = trialInfoDct["TrialName"]
                    NormCorrectBeamResponse = trialInfoDct["NormCorrectBeamResponse"]
                    ModLeft = trialInfoDct["ModLeft"]
                    ModBal = trialInfoDct["ModBal"]
                    ModRight = trialInfoDct["ModRight"]

                    WeightSide = trialInfoDct["WeightSide"]
                    ItemID = trialInfoDct["ItemID"]
                    WeightSideLeft = 1*(WeightSide == "L")

                    pChooseLeft = (1-randomResponseSubj) * ((pUseWeight * WeightSideLeft) + (1-pUseWeight) * ModLeft) + randomResponseSubj/3

                    pChooseBal =  (1-randomResponseSubj) * (1-pUseWeight) * ModBal + randomResponseSubj/3
                    pChooseRight = (1-randomResponseSubj) * (pUseWeight * (1-WeightSideLeft) + (1-pUseWeight) * ModRight) + randomResponseSubj/3



                    #print(pUseWeight, ModLeft, ModBal, ModRight)
                    print(condition, subj, np.round([randomResponseSubj, pChooseLeft, pChooseBal, pChooseRight],2), np.sum([pChooseLeft, pChooseBal, pChooseRight]))

                    choice = ["L","B","R"][np.argmax(multinomial.rvs(n=1, p=[pChooseLeft, pChooseBal, pChooseRight]))]


                    o += f"{condition},{subj},{useWeightGrp_icpt},{weightVar_icpt},{useWeightGrp_lowmem},{weightVar_lowmem},"
                    o += f"{useWeightGrp_highmem},{weightVar_highmem},{randomResponseGrp},{randomScale},{useWeightSubj_icpt},"
                    o += f"{useWeightSubj_lowmem},{useWeightSubj_highmem},{randomResponseSubj},"
                    o += f"{TrialId},{ItemID},{TrialName},{trialType},{cogLoad},{NormCorrectBeamResponse},{WeightSide},{ModLeft},{ModBal},{ModRight},{choice}\n"
                    TrialId += 1


                print(c)
                c += 1


    f = open(f"datasets/fake_dataset_150_{icpt}.csv","w+")
    f.write(o)
    f.close()





