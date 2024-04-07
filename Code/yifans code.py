import pandas as pd
import numpy as np

###################################################################################################################
########## Global Constant Declarations:

# Global constants:
# lapse rate per exposure year
lapse = 0.012346206
# risk premium: added to inflation when discounting 
riskPrem = 0 #0.05
# start of projection
strtYr = 2004
# end of projection
endYr = 2023
endIndicator = 1 # 1 means we want to project to endYr, 0 we project till end of last life
# maximum age allowed by the life table
maxAge = 130
# the minimum annual inflation adjustment amount
minAdj = 0.05

# expense assumptions:
# these are assumptions for end of 2023, so we need to discount them to 2003
# example: in year 2023, every expense is multiplied by a factor of 1/1.03, this is true for both EoY and SoY 
# EoY expenses are discounted according to 
# expense growth rate set at fixed 3% pa
expGrowth = 0.03
# conversion of AUD to Lumarian 
# expAdj = 0.66 * 1.743
# renewal expense split into fixed renewal and % of cover (pa)
renExp = 200
# claim expense is a fixed 8000 per claim
claimExp = 5000
# initial expense is a fixed
initExp = 500


###################################################################################################################
########## Function Declarations:

# gets survival rate vector from 2003 to end
# pv for 2022 is the probability policyholder is still alive at the start of year 2022
def getpv(issueYear, age, dur, data):
    # finds end: the final age we project to for the particular model point
    if endIndicator == 1:
        end = min(age + endYr - issueYear, maxAge)
    else:
        end = maxAge
    # if policy is fixed term
    if dur != -1:
        end = min(age + dur - 1, end)
    # project survival rates for each issueYstarting from joining age
    qvTotal = data.loc[age:end].to_numpy()
    p = 1
    pvTotal = []
    for q in qvTotal:
        pvTotal.append(p)
        p = max(1 - q, 0) * (1 - lapse) * p # as after the adjustment some mort rates might be > 1, we put in a 0 floor for 1 - q
    pvTotal = np.array(pvTotal)
    # find starting age (as an index of the array)
    start = max(0, strtYr - issueYear)
    pv = pvTotal[start:]
    return pv

# gets death rates for each year from max(2003, year of starting policy) to end
def getqv(issueYear, age, dur, data):
    start = max(age, age + strtYr - issueYear)
    # finds end: the final age we project to for the particular model point
    if endIndicator == 1:
        end = min(age + endYr - issueYear, maxAge)
    else:
        end = maxAge
    # if policy is fixed term
    if dur != -1:
        end = min(age + dur - 1, end)

    qv = data.loc[start:end].to_numpy()
    return qv

# gets discount factors applied to end of year cashflows
def getDisc(issueYear, age, dur, data):
    start = max(issueYear, strtYr)
    # finds end: the final year we project to for the particular model point
    if endIndicator == 1:
        end = endYr
    else:
        end = issueYear + maxAge - age
    # if policy is fixed term
    if dur != -1:
        end = min(issueYear + dur - 1, end)
    v = data.loc[start:end]
    return v.to_numpy() 


# gets the inflation adjustments from max(2003, policy start year) to 2023
def getAdjustment(issueYear, age, dur):
    start = max(issueYear, strtYr)
    if endIndicator == 1:
        end = endYr
    else:
        end = issueYear + maxAge - age
    # if policy is fixed term
    if dur != -1:
        end = min(issueYear + dur - 1, end)
    return inflationAdjustment.loc[start:end, issueYear].to_numpy()


# function which calculates the expected loss of one customer given fv, mort rates, discount rates, lapse rates
def PVCalc(coverage, pv, qv, discV, adjustments):
    # value by value multiplication to find rate of claim per year
    claimrates = np.multiply(pv, qv)
    # inflation protection adjusted coverage for each year
    coverageAdj = adjustments * coverage
    # find PV benefit payout
    PVBen = np.dot(np.multiply(discV, claimrates), coverageAdj) 
    return PVBen

# prints summary stats for claims outgo and expenses for each age range
def printSummary(ageGroup, data, df):
    totalClaim = data.loc[:,"claim.outgo"].sum()
    avgClaim = data.loc[:,"claim.outgo"].mean()
    totalInitExp = data.loc[:,"initial.exp"].sum()
    avgInitExp = data.loc[:, "initial.exp"].mean()
    totalRenExp = data.loc[:,"renewal.exp"].sum()
    avgRenExp = data.loc[:,"renewal.exp"].mean()
    totalClaimExp = data.loc[:,"claim.exp"].sum()
    avgClaimExp = data.loc[:,"claim.exp"].mean()
    totalPayout = data.loc[:,"total.outgo"].sum()
    avgPayout = data.loc[:,"total.outgo"].mean()

    new_row = {"Group": ageGroup, 
               "Total Claims": totalClaim, "Avg Claims": avgClaim, 
               "Total Initial Exp": totalInitExp, "Avg Initial Exp": avgInitExp, 
               "Total Renewal Exp": totalRenExp, "Average Renewal Exp": avgRenExp, 
               "Total Claim Exp": totalClaimExp, "Average Claim Exp": avgClaimExp, 
               "Total Payout": totalPayout, "Avg Payout": avgPayout}

    if df.empty:
        df = pd.DataFrame([new_row])
    else:
        df.loc[len(df)] = new_row 
        df = df.reset_index(drop=True)
    return df

# gets summary of death numbers in each age group
def deathSummary(ageGroup, data, df):
    totalDeaths0 = data.loc[:,"Death_P0"].sum()
    avgDeaths0 = data.loc[:,"Death_P0"].mean()
    totalDeaths1 = data.loc[:,"Death_P1"].sum()
    avgDeaths1 = data.loc[:,"Death_P1"].mean()
    totalDeaths2 = data.loc[:,"Death_P2"].sum()
    avgDeaths2 = data.loc[:,"Death_P2"].mean()

    new_row = {"Group": ageGroup, 
               "Total Deaths Orig": totalDeaths0, "Avg total Deaths Orig": avgDeaths0, 
               "Total Deaths Low": totalDeaths1, "Avg total Deaths Low": avgDeaths1, 
               "Total Deaths High": totalDeaths2, "Avg Total Deaths High": avgDeaths2}

    if df.empty:
        df = pd.DataFrame([new_row])
    else:
        df.loc[len(df)] = new_row 
        df = df.reset_index(drop=True)
    return df

###################################################################################################################
########## original valuation:

mort = pd.read_excel('mortality.xlsx', index_col = 0)       # mortality rates
disc = pd.read_excel('inflation.xlsx', index_col = 0)       # discounting factors, make sure in numeric not %!
inforce = pd.read_excel('Yifan_Data.xlsx', index_col = 0)    # in force customer data


# calculate the adjustment factors to be applied to expenses incurred from 2003-2023
# expenseAdjustment.loc[2022] is factor multiplied to all expenses incurred in 2022, equal to (1/1.03)^{2}
years = [i for i in range(strtYr, endYr + 1)]
d = 1
df = []
for year in years:
    d = d / (1 + expGrowth)
    df.append(d)
df.reverse()
expenseAdjustment = pd.DataFrame(df, index = years, columns = ["Expense Adjustment Factor"])
# print(expenseAdjustment)

# calculate the yearly growth factors of policies starting from the min start year in our database (2001) to 2023
minYr = 2001 #inforce["Issue.year"].min()
years = [i for i in range(minYr, endYr + 1)]
df = []
for year in years:
    adjFactors = [1 + max(minAdj, disc.loc[i, "Inflation"]) for i in range(year, endYr + 1)]
    adjCumulative = 1
    adjFactorsCumulative = []
    for factor in adjFactors:
        adjFactorsCumulative.append(adjCumulative)
        adjCumulative = adjCumulative * factor

    adjFactorsCumulative = [0 * i for i in range(minYr, year)] + adjFactorsCumulative
    df.append(adjFactorsCumulative)

df = np.array(df)
df = np.transpose(df)
inflationAdjustment = pd.DataFrame(df, index = years, columns = years)
# print(inflationAdjustment)


# calculate the discounting factors for each year using inflation rates
# disc.loc[2022] is the discount factor applied to end of 2022 Cashflow
inflation = disc.loc[strtYr:, "Inflation"]
years = inflation.index
d = 1
discFactor = []
for i in inflation:
    d = 1 / (1 + i + riskPrem) * d
    discFactor.append(d)
disc = pd.DataFrame(data = discFactor, index = years, columns = ["Discount Factor"])

# calculate total expected PV Ben
import time
strt = time.time()

PVClaims = []
PVInitialExpenses = []
PVRenewalExpenses = []
PVClaimExpenses = []
PVTotals = []

for i, policy in inforce.iterrows():
    PVClaim = 0
    PVInitialExpense = 0
    PVRenewalExpense = 0
    PVClaimExpense = 0
    PVTotal = 0

    issueYear = policy["Issue.year"]
    polType = policy["Policy.type"]
    if polType == "T20":
        dur = 20    # 20 year duration
    else:
        dur = -1    # lifetime duration
    age = policy["Issue.age"]
    coverage = policy["Face.amount"]
    mortalityCoeff = policy["explm"]

    # multiply base mortality by Aden's factors
    pv = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff)    # gets prob of surviving to start of each year of the policy
    qv = getqv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff) 
    discEOY = getDisc(issueYear, age, dur, disc["Discount Factor"]) # gets discount factor used for claims 
    coverageAdjustments = getAdjustment(issueYear, age, dur)
    
    # find PV of claims
    claimrates = np.multiply(pv, qv)
    coverageAdjusted = coverageAdjustments * coverage    # inflation protection adjusted coverage for each year
    PVClaim += np.dot(np.multiply(discEOY, claimrates), coverageAdjusted)
    PVClaims.append(PVClaim)

    # find PV of iniital, claims, renewal expenses
    start = max(issueYear, strtYr)
    if endIndicator == 1:
        end = endYr
    else:
        end = issueYear + maxAge - age
    if dur != -1:
        end = min(issueYear + dur - 1, end)

    # PV initial expense
    if issueYear < strtYr:
        # policies starting before 2003 have no initial exp, we calc the expected renewal exp of the first year 
        PVRenewalExpense += renExp * pv[0] * expenseAdjustment.loc[start, "Expense Adjustment Factor"]
    elif issueYear == strtYr:
        PVInitialExpense += initExp * expenseAdjustment.loc[start, "Expense Adjustment Factor"]     # when policy started at 2003, we have undiscounted initial expense paid at start of 2003
    else:
        # if policy started after 2003, find the expected initial expense
        PVInitialExpense += initExp * expenseAdjustment.loc[start, "Expense Adjustment Factor"] \
            * disc.loc[start - 1, "Discount Factor"]
    PVInitialExpenses.append(PVInitialExpense)

    # renewal expenses
    # finds discount factor vector
    # for renewal expenses for cover from 2003 - 2023, renewals go from start 2004 - start 2023, we want discount factors from 2003 - 2022
    renewalDisc = discEOY[:-1]
    # we need pr of surviving from 1st policy anniversary (2nd policy year) to last policy year 
    renewalRates = pv[1:]
    # renewal expenses only start on the first policy anniversary
    renewalExpense = renExp * expenseAdjustment.loc[start + 1:end, "Expense Adjustment Factor"]
    renewalExpense = renewalExpense.to_numpy()
    PVRenewalExpense += np.dot(np.multiply(renewalDisc, renewalRates), renewalExpense)
    PVRenewalExpenses.append(PVRenewalExpense)

    # PV claims expenses
    claimExpense = claimExp * expenseAdjustment.loc[start:end, "Expense Adjustment Factor"]
    claimExpense.to_numpy()
    PVClaimExpense += np.dot(np.multiply(discEOY, claimrates), claimExpense)
    PVClaimExpenses.append(PVClaimExpense)
    
    # PV total outgo
    PVTotal += PVClaim + PVInitialExpense + PVRenewalExpense + PVClaimExpense
    PVTotals.append(PVTotal)

end = time.time()

# add columns of claims outgo and expenses onto the dataframe inforce dataset
inforce["total.outgo"] = PVTotals
inforce["claim.outgo"] = PVClaims
inforce["initial.exp"] = PVInitialExpenses
inforce["renewal.exp"] = PVRenewalExpenses
inforce["claim.exp"] = PVClaimExpenses


# now summary stats for each age group
inforce["Current.age"] = inforce["Issue.age"] + (strtYr - inforce["Issue.year"])   # generate a col for age at start of projection (2003)
inforce["Current.age"][inforce["Current.age"] < inforce["Issue.age"]] = inforce["Issue.age"]    # maybe sus
# sends the updated inforce dataset to excel for checking
# file_name = "inforce_processed.xlsx"
# inforce.to_excel(file_name)

U30 = inforce.query('`Current.age` < 30')
U40 = inforce.query('`Current.age` < 40 and `Current.age` >= 30')
U45 = inforce.query('`Current.age` < 45 and `Current.age` >= 40')
U50 = inforce.query('`Current.age` < 50 and `Current.age` >= 45')
U55 = inforce.query('`Current.age` < 55 and `Current.age` >= 50')
U60 = inforce.query('`Current.age` < 60 and `Current.age` >= 55')
U65 = inforce.query('`Current.age` < 65 and `Current.age` >= 60')
O65 = inforce.query('`Current.age` >= 65')

cols= ["Group", "Total Claims", "Avg Claims", "Total Initial Exp", "Avg Initial Exp", "Total Renewal Exp", "Average Renewal Exp", 
               "Total Claim Exp", "Average Claim Exp", "Total Payout", "Avg Payout"]
summaryTable = pd.DataFrame(columns = cols)

summaryTable = printSummary("total", inforce, summaryTable)
summaryTable = printSummary("under 30", U30, summaryTable)
summaryTable = printSummary("30 - 39", U40, summaryTable)
summaryTable = printSummary("40 - 44", U45, summaryTable)
summaryTable = printSummary("45 - 49", U50, summaryTable)
summaryTable = printSummary("50 - 54", U55, summaryTable)
summaryTable = printSummary("55 - 59", U60, summaryTable)
summaryTable = printSummary("60 - 64", U65, summaryTable)
summaryTable = printSummary("65 +", O65, summaryTable)

summaryTable.to_excel("summary_original.xlsx")
print(f"Done in {end - strt}s")


###################################################################################################################
########## Low Utilisation:
import time
strt = time.time()

PVClaims = []
PVInitialExpenses = []
PVRenewalExpenses = []
PVClaimExpenses = []
PVTotals = []

for i, policy in inforce.iterrows():
    PVClaim = 0
    PVInitialExpense = 0
    PVRenewalExpense = 0
    PVClaimExpense = 0
    PVTotal = 0

    issueYear = policy["Issue.year"]
    polType = policy["Policy.type"]
    if polType == "T20":
        dur = 20    # 20 year duration
    else:
        dur = -1    # lifetime duration
    age = policy["Issue.age"]
    coverage = policy["Face.amount"]
    mortalityCoeff = policy["explm"]
    reductionCoeff = policy["Low_Utilisation_Multiplier"]   # reduction in mort from low utilization of policy

    # multiply base mortality by Aden's factors
    pv = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeff)    # gets prob of surviving to start of each year of the policy
    qv = getqv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeff) 
    discEOY = getDisc(issueYear, age, dur, disc["Discount Factor"]) # gets discount factor used for claims 
    coverageAdjustments = getAdjustment(issueYear, age, dur)
    
    # find PV of claims
    claimrates = np.multiply(pv, qv)
    coverageAdjusted = coverageAdjustments * coverage    # inflation protection adjusted coverage for each year
    PVClaim += np.dot(np.multiply(discEOY, claimrates), coverageAdjusted)
    PVClaims.append(PVClaim)

    # find PV of iniital, claims, renewal expenses
    start = max(issueYear, strtYr)
    if endIndicator == 1:
        end = endYr
    else:
        end = issueYear + maxAge - age
    if dur != -1:
        end = min(issueYear + dur - 1, end)

    # PV initial expense
    if issueYear < strtYr:
        # policies starting before 2003 have no initial exp, we calc the expected renewal exp of the first year 
        PVRenewalExpense += renExp * pv[0] * expenseAdjustment.loc[start, "Expense Adjustment Factor"]
    elif issueYear == strtYr:
        PVInitialExpense += initExp * expenseAdjustment.loc[start, "Expense Adjustment Factor"]     # when policy started at 2003, we have undiscounted initial expense paid at start of 2003
    else:
        # if policy started after 2003, find the expected initial expense
        PVInitialExpense += initExp * expenseAdjustment.loc[start, "Expense Adjustment Factor"] \
            * disc.loc[start - 1, "Discount Factor"]
    PVInitialExpenses.append(PVInitialExpense)

    # renewal expenses
    # finds discount factor vector
    # for renewal expenses for cover from 2003 - 2023, renewals go from start 2004 - start 2023, we want discount factors from 2003 - 2022
    renewalDisc = discEOY[:-1]
    # we need pr of surviving from 1st policy anniversary (2nd policy year) to last policy year 
    renewalRates = pv[1:]
    # renewal expenses only start on the first policy anniversary
    renewalExpense = renExp * expenseAdjustment.loc[start + 1:end, "Expense Adjustment Factor"]
    renewalExpense = renewalExpense.to_numpy()
    PVRenewalExpense += np.dot(np.multiply(renewalDisc, renewalRates), renewalExpense)
    PVRenewalExpenses.append(PVRenewalExpense)

    # PV claims expenses
    claimExpense = claimExp * expenseAdjustment.loc[start:end, "Expense Adjustment Factor"]
    claimExpense.to_numpy()
    PVClaimExpense += np.dot(np.multiply(discEOY, claimrates), claimExpense)
    PVClaimExpenses.append(PVClaimExpense)
    
    # PV total outgo
    PVTotal += PVClaim + PVInitialExpense + PVRenewalExpense + PVClaimExpense
    PVTotals.append(PVTotal)

end = time.time()

# add columns of claims outgo and expenses onto the dataframe inforce dataset
inforce["total.outgo"] = PVTotals
inforce["claim.outgo"] = PVClaims
inforce["initial.exp"] = PVInitialExpenses
inforce["renewal.exp"] = PVRenewalExpenses
inforce["claim.exp"] = PVClaimExpenses


# now summary stats for each age group
inforce["Current.age"] = inforce["Issue.age"] + (strtYr - inforce["Issue.year"])   # generate a col for age at start of projection (2003)
inforce["Current.age"][inforce["Current.age"] < inforce["Issue.age"]] = inforce["Issue.age"]
# sends the updated inforce dataset to excel for checking
# file_name = "inforce_processed.xlsx"
# inforce.to_excel(file_name)

U30 = inforce.query('`Current.age` < 30')
U40 = inforce.query('`Current.age` < 40 and `Current.age` >= 30')
U45 = inforce.query('`Current.age` < 45 and `Current.age` >= 40')
U50 = inforce.query('`Current.age` < 50 and `Current.age` >= 45')
U55 = inforce.query('`Current.age` < 55 and `Current.age` >= 50')
U60 = inforce.query('`Current.age` < 60 and `Current.age` >= 55')
U65 = inforce.query('`Current.age` < 65 and `Current.age` >= 60')
O65 = inforce.query('`Current.age` >= 65')

cols= ["Group", "Total Claims", "Avg Claims", "Total Initial Exp", "Avg Initial Exp", "Total Renewal Exp", "Average Renewal Exp", 
               "Total Claim Exp", "Average Claim Exp", "Total Payout", "Avg Payout"]
summaryTable = pd.DataFrame(columns = cols)

summaryTable = printSummary("total", inforce, summaryTable)
summaryTable = printSummary("under 30", U30, summaryTable)
summaryTable = printSummary("30 - 39", U40, summaryTable)
summaryTable = printSummary("40 - 44", U45, summaryTable)
summaryTable = printSummary("45 - 49", U50, summaryTable)
summaryTable = printSummary("50 - 54", U55, summaryTable)
summaryTable = printSummary("55 - 59", U60, summaryTable)
summaryTable = printSummary("60 - 64", U65, summaryTable)
summaryTable = printSummary("65 +", O65, summaryTable)

summaryTable.to_excel("summary_lowUtilization.xlsx")
print(f"Done in {end - strt}s")


###################################################################################################################
########## High Utilisation:
import time
strt = time.time()

PVClaims = []
PVInitialExpenses = []
PVRenewalExpenses = []
PVClaimExpenses = []
PVTotals = []

for i, policy in inforce.iterrows():
    PVClaim = 0
    PVInitialExpense = 0
    PVRenewalExpense = 0
    PVClaimExpense = 0
    PVTotal = 0

    issueYear = policy["Issue.year"]
    polType = policy["Policy.type"]
    if polType == "T20":
        dur = 20    # 20 year duration
    else:
        dur = -1    # lifetime duration
    age = policy["Issue.age"]
    coverage = policy["Face.amount"]
    mortalityCoeff = policy["explm"]
    reductionCoeff = policy["High_Utilisation_Multiplier"]   # reduction in mort from low utilization of policy

    # multiply base mortality by Aden's factors
    pv = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeff)    # gets prob of surviving to start of each year of the policy
    qv = getqv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeff) 
    discEOY = getDisc(issueYear, age, dur, disc["Discount Factor"]) # gets discount factor used for claims 
    coverageAdjustments = getAdjustment(issueYear, age, dur)
    
    # find PV of claims
    claimrates = np.multiply(pv, qv)
    coverageAdjusted = coverageAdjustments * coverage    # inflation protection adjusted coverage for each year
    PVClaim += np.dot(np.multiply(discEOY, claimrates), coverageAdjusted)
    PVClaims.append(PVClaim)

    # find PV of iniital, claims, renewal expenses
    start = max(issueYear, strtYr)
    if endIndicator == 1:
        end = endYr
    else:
        end = issueYear + maxAge - age
    if dur != -1:
        end = min(issueYear + dur - 1, end)

    # PV initial expense
    if issueYear < strtYr:
        # policies starting before 2003 have no initial exp, we calc the expected renewal exp of the first year 
        PVRenewalExpense += renExp * pv[0] * expenseAdjustment.loc[start, "Expense Adjustment Factor"]
    elif issueYear == strtYr:
        PVInitialExpense += initExp * expenseAdjustment.loc[start, "Expense Adjustment Factor"]     # when policy started at 2003, we have undiscounted initial expense paid at start of 2003
    else:
        # if policy started after 2003, find the expected initial expense
        PVInitialExpense += initExp * expenseAdjustment.loc[start, "Expense Adjustment Factor"] \
            * disc.loc[start - 1, "Discount Factor"]
    PVInitialExpenses.append(PVInitialExpense)

    # renewal expenses
    # finds discount factor vector
    # for renewal expenses for cover from 2003 - 2023, renewals go from start 2004 - start 2023, we want discount factors from 2003 - 2022
    renewalDisc = discEOY[:-1]
    # we need pr of surviving from 1st policy anniversary (2nd policy year) to last policy year 
    renewalRates = pv[1:]
    # renewal expenses only start on the first policy anniversary
    renewalExpense = renExp * expenseAdjustment.loc[start + 1:end, "Expense Adjustment Factor"]
    renewalExpense = renewalExpense.to_numpy()
    PVRenewalExpense += np.dot(np.multiply(renewalDisc, renewalRates), renewalExpense)
    PVRenewalExpenses.append(PVRenewalExpense)

    # PV claims expenses
    claimExpense = claimExp * expenseAdjustment.loc[start:end, "Expense Adjustment Factor"]
    claimExpense.to_numpy()
    PVClaimExpense += np.dot(np.multiply(discEOY, claimrates), claimExpense)
    PVClaimExpenses.append(PVClaimExpense)
    
    # PV total outgo
    PVTotal += PVClaim + PVInitialExpense + PVRenewalExpense + PVClaimExpense
    PVTotals.append(PVTotal)

end = time.time()

# add columns of claims outgo and expenses onto the dataframe inforce dataset
inforce["total.outgo"] = PVTotals
inforce["claim.outgo"] = PVClaims
inforce["initial.exp"] = PVInitialExpenses
inforce["renewal.exp"] = PVRenewalExpenses
inforce["claim.exp"] = PVClaimExpenses


# now summary stats for each age group
inforce["Current.age"] = inforce["Issue.age"] + (strtYr - inforce["Issue.year"])   # generate a col for age at start of projection (2003)
inforce["Current.age"][inforce["Current.age"] < inforce["Issue.age"]] = inforce["Issue.age"]
# sends the updated inforce dataset to excel for checking
# file_name = "inforce_processed.xlsx"
# inforce.to_excel(file_name)

U30 = inforce.query('`Current.age` < 30')
U40 = inforce.query('`Current.age` < 40 and `Current.age` >= 30')
U45 = inforce.query('`Current.age` < 45 and `Current.age` >= 40')
U50 = inforce.query('`Current.age` < 50 and `Current.age` >= 45')
U55 = inforce.query('`Current.age` < 55 and `Current.age` >= 50')
U60 = inforce.query('`Current.age` < 60 and `Current.age` >= 55')
U65 = inforce.query('`Current.age` < 65 and `Current.age` >= 60')
O65 = inforce.query('`Current.age` >= 65')

cols= ["Group", "Total Claims", "Avg Claims", "Total Initial Exp", "Avg Initial Exp", "Total Renewal Exp", "Average Renewal Exp", 
               "Total Claim Exp", "Average Claim Exp", "Total Payout", "Avg Payout"]
summaryTable = pd.DataFrame(columns = cols)

summaryTable = printSummary("total", inforce, summaryTable)
summaryTable = printSummary("under 30", U30, summaryTable)
summaryTable = printSummary("30 - 39", U40, summaryTable)
summaryTable = printSummary("40 - 44", U45, summaryTable)
summaryTable = printSummary("45 - 49", U50, summaryTable)
summaryTable = printSummary("50 - 54", U55, summaryTable)
summaryTable = printSummary("55 - 59", U60, summaryTable)
summaryTable = printSummary("60 - 64", U65, summaryTable)
summaryTable = printSummary("65 +", O65, summaryTable)

summaryTable.to_excel("summary_HighUtilization.xlsx")
print(f"Done in {end - strt}s")


###################################################################################################################
########## Find expected death numbers:
deaths0 = []
deaths1 = []
deaths2 = []

for i, policy in inforce.iterrows():
    issueYear = policy["Issue.year"]
    polType = policy["Policy.type"]
    if polType == "T20":
        dur = 20    # 20 year duration
    else:
        dur = -1    # lifetime duration
    age = policy["Issue.age"]
    coverage = policy["Face.amount"]
    mortalityCoeff = policy["explm"]
    reductionCoeffLow = policy["Low_Utilisation_Multiplier"]
    reductionCoeffHigh = policy["High_Utilisation_Multiplier"]

    # multiply base mortality by Aden's factors
    pv0 = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff)    # gets prob of surviving to start of each year of the policy
    qv0 = getqv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff) 

    pv1 = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeffLow) 
    qv1 = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeffLow) 

    pv2 = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeffHigh) 
    qv2 = getpv(issueYear, age, dur, mort["Mortality Rate"] * mortalityCoeff * reductionCoeffHigh) 

    death0 = np.dot(pv0, qv0)
    death1 = np.dot(pv1, qv1)
    death2 = np.dot(pv2, qv2)

    deaths0.append(death0)
    deaths1.append(death1)
    deaths2.append(death2)

inforce["Death_P0"] = deaths0
inforce["Death_P1"] = deaths1
inforce["Death_P2"] = deaths2

inforce.to_excel("Inforce_with_deaths.xlsx")

U30 = inforce.query('`Current.age` < 30')
U40 = inforce.query('`Current.age` < 40 and `Current.age` >= 30')
U45 = inforce.query('`Current.age` < 45 and `Current.age` >= 40')
U50 = inforce.query('`Current.age` < 50 and `Current.age` >= 45')
U55 = inforce.query('`Current.age` < 55 and `Current.age` >= 50')
U60 = inforce.query('`Current.age` < 60 and `Current.age` >= 55')
U65 = inforce.query('`Current.age` < 65 and `Current.age` >= 60')
O65 = inforce.query('`Current.age` >= 65')

cols= ["Group", "Total Deaths Orig", "Avg total Deaths Orig", "Total Deaths Low", 
       "Avg total Deaths Low", "Total Deaths High", "Avg Total Deaths High"]
summaryTable = pd.DataFrame(columns = cols)

summaryTable = deathSummary("total", inforce, summaryTable)
summaryTable = deathSummary("under 30", U30, summaryTable)
summaryTable = deathSummary("30 - 39", U40, summaryTable)
summaryTable = deathSummary("40 - 44", U45, summaryTable)
summaryTable = deathSummary("45 - 49", U50, summaryTable)
summaryTable = deathSummary("50 - 54", U55, summaryTable)
summaryTable = deathSummary("55 - 59", U60, summaryTable)
summaryTable = deathSummary("60 - 64", U65, summaryTable)
summaryTable = deathSummary("65 +", O65, summaryTable)

summaryTable.to_excel("Deaths Summary.xlsx")

print("done")
