#!/usr/bin/python3

import csv, math, sys, argparse, random,os,errno
import numpy as np
import logging
import time
import subprocess
from ceecexp import Experiment

class TophatPrior(object):
    """
    Tophat prior
    
    :param min: scalar or array of min values
    :param max: scalar or array of max values
    """
    
    def __init__(self, min, max):
        self.min = np.atleast_1d(min)
        self.max = np.atleast_1d(max)
        self._random = np.random.mtrand.RandomState()
        assert self.min.shape == self.max.shape
        assert np.all(self.min < self.max)
        
    def __call__(self, theta=None):
        if theta is None:
            return np.array([self._random.uniform(mi, ma) for (mi, ma) in zip(self.min, self.max)])
        else:
            return 1 if np.all(theta < self.max) and np.all(theta >= self.min) else 0



##Compute the distance from the exp to the real data
def dist(x,y):
    diff= x-y;
    return diff

#generate a pool of experiment of size `size` that will be stored in the folder `pref`
def genTestPool(size,pref):
    pool_exp={}
    for p in range(size):
        priors = TophatPrior([0,300,5],[1,1000,7])
        params=priors()
        one=Experiment(params,"/home/bsc21/bsc21394/ceeculture/",pref)
        with open("totry.out","a") as ttexp:
            ttexp.write(one.particleDirectory+'\n')
        pool_exp[one.getId()]=one
    return(pool_exp)



###Write the task file and update the counter of the number of task per file
def writeNupdate(tmp_pdict):
    global countExpKind
    global countFileKind
    global tasks
    task_per_node=20 #defined given MN configuration
    
    for pone in tmp_pdict.keys() :
        one=tmp_pdict[pone]
        kind=one.getKind()
        task=one.generateTask()

        if( not( kind in countExpKind.keys())): #check if this kind is already recorded
            countExpKind[kind]=0 
            countFileKind[kind]=0

        countExpKind[kind]=countExpKind[kind]+1 #increase number of expe of this kind

        if(countExpKind[kind] > task_per_node): #if number of expe is too high, increase number of file 
            #TODO here should launch the file already full fullfillfillfull
            countFileKind[kind]=countFileKind[kind]+1
            countExpKind[kind]=0


        if (not os.path.isdir("taskfiles")):
            os.makedirs("taskfiles") #create folder for the taskfiles

        taskfilename=kind+"_"+str(countFileKind[kind])+".task"
        taskfilename=os.path.join("taskfiles",taskfilename)

        with open(taskfilename,'a') as tskf:
            tskf.write(task)

        tasks[taskfilename]=True

###Start the experiment carefull as it is strongly palteform dependent
def launchExpe(taskfile):
    command = "sbatch mn4_manual_scheduling.sh "+taskfile)
    process = subprocess.Popen(bashCommand, stdout=subprocess.PIPE,shell=True)
     


if __name__ == '__main__' :


    pdict={}     #list of score for each exp
    countExpKind={} #number of experiment for each different "kind" 
    countFileKind={} #number of tasksfile for each different "kind" 
    tasks={} #list of taskfiles that have to be send to mn

    tmp_pdict={} #pool of particules
    numParticule=int(sys.argv[1]) #This is the total number of  particule (aka Thetas, aka set of parameter) that we want
    numproc=int(sys.argv[2]) #this is the number of parallele task we will try
    numproc_node=int(sys.argv[3]) #this is the number of parallele task we will try
    epsilon=int(sys.argv[4])  #the maximum score we accept (o minimum)

    orign=os.getcwd()
    
    pref="eps_"+str(epsilon)

    with open("totry.out","w") as ttexp:
        ttexp.write("")
   
    tmp_pdict=genTestPool(numproc,pref)
    ###initialize pool
    writeNupdate(tmp_pdict)

    ##findFileneNameAndUpdateCounter
    #Launch remaining tasks
       
    while(len(pdict) < numParticule):
        tsks=list(tasks.keys())
        if(len(tsks)>0):
            print(tsks)
            for l in tsks:
                launchExpe(l)
                tasks.pop(l,None)
            for cnt in countFileKind.keys():
                countFileKind[cnt]=countFileKind[cnt]+1


        ##update the pool of particule given their score if the experiment has finished
        tmp_keys=list(tmp_pdict.keys())
        for t in tmp_keys:
            tmp_exp=tmp_pdict[t]
            tmp_exp.gatherScore()
            if(tmp_exp.score>0):
                if(tmp_exp.score > epsilon):
                    tmp_exp.remove()
                    tmp_pdict.pop(t,None)
                else:
                    pdict[tmp_exp.getId()]=tmp_exp.score
                    tmp_pdict.pop(t,None)

        #the pool is empty : all simulation finished and we have not yeat enough particle
        if(len(tmp_pdict) == 0): 
            with open("totry.out","w") as ttexp:
                ttexp.write("")

            ###re-initialize pool
            tmp_pdict=genTestPool(numproc,pref)
            writeNupdate(tmp_pdict)
            ##findFileneNameAndUpdateCounter
            #Launch remaining tasks
