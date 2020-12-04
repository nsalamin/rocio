#!/usr/bin/python

import sys
import numpy as np
from scipy.stats import gamma

# File headers
hdrPair = ["Pair", "Stat", "RCmin", "PRmin", "Nmin", "PValue", "Nsim"]
hdrPosition = ["Position", "IsComplete", "IsConstant", "RC", "PR", "N", "logLn"]
hdrSimu =  ["Stat", "RCmin", "PRmin", "Nmin"]

def readPositions(positionFile):

    inFile = open(positionFile)

    headers = inFile.readline()

    data = []
    for line in inFile:
        line = line.strip()
        words = line.split()
        words[0] = words[0][1:-1]
        row = [float(w) for w in words]
        data.append(row)

    return data

def readSimus(simuFile):

    inFile = open(simuFile)

    headers = inFile.readline()

    data = []
    for line in inFile:
        line = line.strip()
        words = line.split()
        row = [float(w) for w in words]
        data.append(row)

    return data

def readPairs(pairFile):
    inFile = open(pairFile)

    headers = inFile.readline()

    data = []
    for line in inFile:
        line = line.strip()
        words = line.split()
        pair = [int(p) for p in words[0][1:-1].split(";")]

        corr = float(words[1])
        rateClassMin = float(words[2])
        posteriorRateMin = float(words[3])
        normRateMin = float(words[4])
        pVal = float(words[5])
        nSim = int(words[6])

        row = [pair, corr, rateClassMin, posteriorRateMin, normRateMin, pVal, nSim]
        data.append(row)

    return data

def defineNRange(posData):
    minN = 0.
    maxN = 0.

    for row in posData:
        N = row[hdrPosition.index("N")]
        maxN = maxN if maxN >= N else N

    return [minN, maxN]

def isPairConserved(pairPos, posData):
    IDX_IS_CONST = hdrPosition.index("IsConstant")
    return posData[pairPos[0]-1][IDX_IS_CONST] == 1 and posData[pairPos[1]-1][IDX_IS_CONST] == 1

def isPairCoev(pairPos, N_PAIR_COEV_PER_DATASET):
    return (pairPos[1] % 2 == 0) and pairPos[1]-pairPos[0] == 1 and pairPos[1] <= 2*N_PAIR_COEV_PER_DATASET

def extractSimulatedResults(simuData, removeConserved=False):

    # hdrSimu =  ["Stat", "RCmin", "PRmin", "Nmin"]
    simu = { "PR" : [], "N" : [], "corr" : [], "color" : []}

    for row in simuData:
        if not removeConserved :
            simu["PR"].append(row[hdrSimu.index("PRmin")])
            simu["N"].append(row[hdrSimu.index("Nmin")])
            simu["corr"].append(row[hdrSimu.index("Stat")])
        elif row[hdrSimu.index("Nmin")] > 1.e-3:
            simu["PR"].append(row[hdrSimu.index("PRmin")])
            simu["N"].append(row[hdrSimu.index("Nmin")])
            simu["corr"].append(row[hdrSimu.index("Stat")])

    return simu

def classifySimulations(boundaries, simuData):

    simuBins = [ [] for i in range(len(boundaries)-1)]

    IDX_NMIN = hdrSimu.index("Nmin")
    IDX_CORR = hdrSimu.index("Stat")

    # Classify simulation per bins
    for row in simuData:
        indexBin = None
        for iB in range(len(boundaries)-1):
            #print IDX_NMIN, " - ", iB, " - ", boundaries, " - ", row
            if row[IDX_NMIN] < boundaries[iB+1]:
                indexBin = iB
                break

        if indexBin == None:
            print "Bin error: boundaries=", boundaries, " - row=", row, " - NMin=", row[IDX_NMIN]
        else:
            simuBins[indexBin].append(row)

    for iB in range(len(simuBins)):
        simuBins[iB].sort(key=lambda row: row[IDX_CORR], reverse=True)

    return simuBins

def findCorrelationThres(simuBins, pValue):

    thresholdPerBin = []

    IDX_CORR = hdrSimu.index("Stat")

    for iB in range(len(simuBins)):
        if not simuBins[iB]: # check if empty
            thresholdPerBin.append(None)
            continue
        elif 1./float(len(simuBins[iB])) >= pValue:
            print "PValue error: min pValue = ", 1./float(len(simuBins[iB]))
            thresholdPerBin.append(None)
            continue

        num = 0
        denom=float(len(simuBins[iB]))

        for row in simuBins[iB]:
            num += 1
            if float(num+1)/float(denom+1) >= pValue:
                thresholdPerBin.append(row[IDX_CORR])
                break

    return thresholdPerBin


def computePValue(boundaries, binnedSimus, Nmin, correlation):

    # Identify the bin
    indexBin = None
    for iB in range(len(boundaries)-1):
        #print IDX_NMIN, " - ", iB, " - ", boundaries, " - ", row
        if Nmin < boundaries[iB+1]:
            indexBin = iB
            break

    if indexBin == None:
        print "Cannot find a bin for Nmin=", Nmin, " - bins=", boundaries
        quit()

    # compute the pValue
    IDX_CORR = hdrSimu.index("Stat")

    num = 0
    denom=float(len(binnedSimus[iB]))

    for row in binnedSimus[iB]:

        if correlation >= row[IDX_CORR]:
            pVal = float(num+1)/float(denom+1)
            return pVal

        num += 1

    return 1.


def recomputePValues(nBins, posData, pairData, simuData, newPairFileName):

    rangeN = defineNRange(posData)
    # Default boundaries

    # Define gamma quantiles
    simu = extractSimulatedResults(simuData, True)
    boundariesGammaN = []
    meanSimuN = np.average(simu["N"])
    varSimuN = np.var(simu["N"])
    betaGamma = meanSimuN/varSimuN
    alphaGamma = meanSimuN*betaGamma

    percentiles = np.linspace(0., 1.0, nBins+1)
    quantiles =  gamma.ppf(percentiles[1:-1], alphaGamma, loc=0, scale=1./betaGamma)
    boundariesGammaN = [rangeN[0]]
    for q in quantiles: boundariesGammaN.append(q)
    if rangeN[1] < quantiles[-1]:
        print "quantile problem:"
        print "Mean: ", meanSimuN
        print "var: ", varSimuN
        print "Alpha: ", alphaGamma
        print "Beta: ", betaGamma
        print "rangeN: ", rangeN
        print "percentile: ", percentiles
        print "boundaries: ", boundariesGammaN
    else:
        boundariesGammaN.append(rangeN[1])

    # Define uniform bins
    boundariesUniformN = np.linspace(0., rangeN[-1], nBins+1)

    # Define equilibred bins
    sortedSimu = sorted(simu["N"])
    slices = [int(s) for s in np.linspace(0., len(sortedSimu)-1, nBins+1)]
    boundariesBalanced = [0]
    for iS in slices[1:-1]:
        boundariesBalanced.append(sortedSimu[iS]+(sortedSimu[iS]-sortedSimu[iS-1])/2.)
    boundariesBalanced.append(rangeN[-1])

    # Bin simus (Unif + Gamma)
    binnedUniformSimu = classifySimulations(boundariesUniformN, simuData)
    binnedGammaSimu = classifySimulations(boundariesGammaN, simuData)
    binnedBalancedSimu = classifySimulations(boundariesBalanced, simuData)

    IDX_CORR = hdrPair.index("Stat")
    IDX_NMIN = hdrPair.index("Nmin")

    outFile = file(newPairFileName, 'w')

    header = ["Group", "Stat", "RCmin", "PRmin", "Nmin", "PValue", "Nsim", "PValueUnif", "PValueGamma", "PValueBalanced"]
    outFile.write("\t".join(header) + "\n")

    for row in pairData:

        correlation = row[IDX_CORR]
        Nmin = row[IDX_NMIN]

        pValUniform = computePValue(boundariesUniformN, binnedUniformSimu, Nmin, correlation)
        pValGamma = computePValue(boundariesGammaN, binnedGammaSimu, Nmin, correlation)
        pValBalanced = computePValue(boundariesBalanced, binnedBalancedSimu, Nmin, correlation)

        outFile.write( "[" + repr(row[0][0]) + ";" + repr(row[0][1]) + "]\t" )
        outFile.write( "\t".join( [ repr(x) for x in row[1:] ] ) )
        outFile.write( "\t" + repr(pValUniform) + "\t" + repr(pValGamma) + "\t" + repr(pValBalanced) + "\n" )

    outFile.close()

# Number of bins for Nmin
nBins = int(sys.argv[1])
# e.g.: "/home/meyerx/Projets/CoevBenchmarks/Simulations"
simFolder = sys.argv[2]

# Default file names -> Change to match your implementation
POSITION_FILE_NAME="out_map_infos.txt"
PAIR_FILE_NAME="out_stats.txt"
SIMU_FILE_NAME="out_stats_null.txt"

# Output file name (with new pValues)
NEW_PAIR_FILE_NAME="out_stats_recomp.txt"

# File paths
positionFileName = "{}/{}".format(simFolder, POSITION_FILE_NAME)
pairFileName = "{}/{}".format(simFolder, PAIR_FILE_NAME)
simuFileName = "{}/{}".format(simFolder, SIMU_FILE_NAME)

# Read file contents
posData = readPositions(positionFileName)
pairData = readPairs(pairFileName)
simuData = readSimus(simuFileName)

# Recompute pValues
newPairFileName = "{}/{}".format(simFolder, NEW_PAIR_FILE_NAME)
recomputePValues(nBins, posData, pairData, simuData, newPairFileName)
