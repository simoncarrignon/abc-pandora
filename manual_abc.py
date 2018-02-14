#!/usr/bin/python3
#TODO : a Class Launcher and a Class Tasks (a class task being a set of experiments , a class launcher an abstract class implemented for each architecture (MN[3-4]/LAPTOP/HSELDON...)

import csv, math, sys, argparse, random,os,errno,re
import numpy as np
import logging
import time
import subprocess
from scipy import stats
from sampler import TophatPrior
from sampler import weighted_cov
from simpleMod  import Experiment
from simpleMod  import genData
from simpleMod  import order


#generate a pool of a numer of `N` experiments that will be stored in the folder `pref`
def genTestPool(prior,N,pref):
    pool_exp={}
    for p in range(N):
        params=priors()
        one=Experiment(params,pref)
        while(not one.consistence):
            params=priors()
            one=Experiment(params,pref)
        pool_exp[one.getId()]=one
    return(pool_exp)

def rawMatricesFromPool(pool):
    rawmat={}
    rawmat["scores"]=[]
    rawmat["thetas"]=[]
    for exp in pool.values():
        rawmat["scores"].append(1.0/numParticule)
        rawmat["thetas"].append(np.array(exp.params))

    rawmat["scores"]=np.asarray(rawmat["scores"])
    rawmat["thetas"]=np.asarray(rawmat["thetas"])
    return(rawmat)

#recreate a subset of N experiments with paramters drawn from distribution of poldpool
#as genTestPool return a dictionnary id=>exp
def renewPool(N,pref,oldpool):
    pool_exp={}
    for p in range(N):
        idx = np.random.choice(range(len(oldpool["ws"])), 1, p=oldpool["ws"]/np.sum(oldpool["ws"]))[0]
        theta = oldpool["thetas"][idx]
        sigma = oldpool["sigma"]
        params = np.random.multivariate_normal(theta, sigma)

        while (params<0).any():
            params = np.random.multivariate_normal(theta, sigma)

        one=Experiment(params,pref)
        while(not one.consistence):
            idx = np.random.choice(range(len(oldpool["ws"])), 1, p=oldpool["ws"]/np.sum(oldpool["ws"]))[0]
            theta = oldpool["thetas"][idx]
            sigma = oldpool["sigma"]
            params = np.random.multivariate_normal(theta, sigma)
            while (params<0).any():
                params = np.random.multivariate_normal(theta, sigma)
            one=Experiment(params,pref)
        pool_exp[one.getId()]=one
    return(pool_exp)



###Write the task file and update the counter of the number of task per file
def writeNupdate(tmp_pdict):

    ###ALL that would  be  really  nicer if ABC was an object 
    ###and wrapper to
    global countExpKind
    global countFileKind
    global tasks
    global numproc_node #defined given MN configuration
    global jobid


    for pone in tmp_pdict.keys() :
        one=tmp_pdict[pone]
        kind=one.getKind()
        task=one.generateTask()

        if( not( kind in countExpKind.keys())): #check if this kind is already recorded
            countExpKind[kind]=0 
            countFileKind[kind]=0

        countExpKind[kind]=countExpKind[kind]+1 #increase number of expe of this kind

        if(countExpKind[kind] > numproc_node): #if number of expe is too high, increase number of file 
            #TODO here should launch the file already full fullfillfillfull
            countFileKind[kind]=countFileKind[kind]+1
            countExpKind[kind]=0


        if (not os.path.isdir("taskfiles")):
            os.makedirs("taskfiles") #create folder for the taskfiles

        taskid=kind+"_"+str(countFileKind[kind])
        taskfilename=taskid+"-"+jobid+".task"
        taskfilename=os.path.join("taskfiles",taskfilename)

        with open(taskfilename,'a') as tskf:
            tskf.write(task)

    #TODO task should be handled as object
        tasks[taskid]={}
        tasks[taskid]['filename']=taskfilename
        tasks[taskid]['status']='hold'
        tasks[taskid]['kind']=kind
    logging.debug(tasks)

###launch batch of experiments given the machine used
#TODO a real class "launcher" that can abstract that for the ABC
def launchExpe(taskfile):
    t=1 #time in minutes (ie for 1h30: t=150)
    h=t/60
    m=t-60*h
    s=0
    subtime=""
    if(os.getenv('BSC_MACHINE') == 'mn4'):
        subtime=":".join([str(h).zfill(2),str(m).zfill(2),str(s).zfill(2)])
        command = "bash 2mn4.sh"
    elif(os.getenv('BSC_MACHINE') == 'nord3'):
        subtime=":".join([str(h).zfill(2),str(m).zfill(2)]) #nord3 there is no seconde
        command = "bash 2nord3.sh"
    else:
        command = "echo"

    command = " ".join([command,taskfile,subtime,str(numproc_node)])
    process = subprocess.Popen(command, stdout=subprocess.PIPE,shell=True)
    #time.sleep()
    return(process)


## Write  a dictionnary of particules `particules` for the epsilon `epsi` in the file `outfilename`
#TODO set a better handling of the header
def writeParticules(particules,epsi,outfilename):
    sep=","
    with open(outfilename, 'wb') as outpart:
        header=order+sep+"score"+sep+'epsilon'+"\n"
        outpart.write(header)
        for eid, score in particules.items():
            thetas=eid.replace("_",",")
            row=thetas+ sep+ str(score) + sep + str(epsi)+ "\n"
            outpart.write(row)

########################################
#MAIN PROGRAM
# use:
#python ./manual_abc.py numParticule numpart numproc_node epsilon
#where :
#* `numParticule`: total number of  particule (aka Thetas, aka set of parameter) that you to draw your distribution (the bigger, the better)
#* `numpart`: number of particules we will generate and check at the same 
#* `numproc_node`: number of parallele task we run in one marenostrum node  (should be < numpart as the idea is , from the numpart generated by the python script we split and  run them in separated nodes)
#* `epsilon`: the maximum score we accept (o minimum) for the particule. _ie_ all particule should have a score < epsilon
#



if __name__ == '__main__' :
    Y=genData()
    print(Y)
    pdict={}     #list of score for each exp
    newpool={}     #list of score for each exp
    countExpKind={} #number of experiment for each different "kind" 
    countFileKind={} #number of tasksfile for each different "kind" 
    tasks={} #list of taskfiles that have to be send to mn, it allows to separate the experiments in different sbatch given some conditions
    tmp_pdict={} #temporary pool of particules
    numParticule=int(sys.argv[1]) #This is the total number of  particule (aka Thetas, aka set of parameter) that we want
    numproc=int(sys.argv[2]) #this is the number of parallele task we will try
    numproc_node=int(sys.argv[3]) #this is the number of parallele task we will try
    #epsilon=float(sys.argv[4])  #the maximum score we accept (o minimum)
    orign=os.getcwd() #original working directory
    jobid="mother_" #the id of the main job (the one that will launch the job that will launch the job) is : mother_pid_sid where pid is the id of the main process (ie gien by the os running the main process) and sid is the id of task as given by the launcher (slurm or whatever)
    jobid+=str(os.getpid())
    try:
        jobid+="_"+os.getenv("SLURM_JOBID")
    except:
        print('not a slurm job')

    numeps=5
    maxeps=0.25
    mineps=0.11
    epsilons=np.logspace(np.log10(maxeps),np.log10(mineps),numeps)

    pref="eps_"+str(np.round(epsilons[0])) #this prefix is mainly use to store the data


    #open a general log file
    logging.basicConfig(format="%(asctime)s;%(levelname)s;%(message)s",filename=str(jobid)+".log",level=logging.INFO)

    priors = TophatPrior([1,1,0],[80,15,1])
    tmp_pdict=genTestPool(priors,numParticule,pref) #tmp_pdict is a dictionnary with the id of an exeriment and the full Experiment obpect 
    firstWeight = np.ones(numParticule) / numParticule
    oldpool=rawMatricesFromPool(tmp_pdict) #oldpool will store only np.array equivalent to the raw data in genTestPool
    oldpool["ws"]=firstWeight
    oldpool["sigma"]=2 * weighted_cov(oldpool["thetas"],oldpool["ws"])

    isNeedLauncher=False
    

    for epsilon in epsilons:
        if(epsilon>1):
            epsilon=np.round(epsilon)
        else:
            epsilon=np.round(epsilon,decimals=4)
        print("esp"+str(epsilon))

        with open("tmp_res"+str(epsilon)+".csv",'w') as tmp_out:
            tmp_out.write("id,score\n")
            tmp_out.close()

        ###initialize pool
        if( isNeedLauncher):
            writeNupdate(tmp_pdict)

        ##findFileneNameAndUpdateCounter
        #Launch remaining tasks

        oldlen=0
        while(len(pdict) < numParticule):

            if(len(pdict)>oldlen): ##logging only if new particules found
                oldlen=len(pdict)
                logging.info(str(len(pdict))+ "/"+str(numParticule)+ " tot")

            dead=0

            ##################################################:LAUNCHING SLURM
            if(isNeedLauncher):
                for tid,tproc in tasks.items():
                    ##check status of the task
                    #if on hold it means it has been created during previous loop and has to be launched
                    if(tasks[tid]['status'] == 'hold'):
                        launcher=launchExpe(tasks[tid]['filename'])
                        out, err = launcher.communicate()
                        logging.info(out)
                        try:
                            remote_id=""
                            if(os.getenv('BSC_MACHINE') == 'mn4'):
                                remote_id=re.search('Submitted batch job ([0-9]+)\n',out).group(1)
                            if(os.getenv('BSC_MACHINE') == 'nord3'):
                                remote_id=re.search('Job <([0-9]+)> is submitted',out).group(1)
                            tasks[tid]['status'] = 'running'
                            tasks[tid]['remote_id'] = remote_id
                        except:
                            logging.warning("Task ID not found")
                            tasks[tid]['status'] = ''
                            logging.warning('probleme while launching the job')
                        #if the task is running (meaning a greasy job has been launched) we check if the job is still running
                        # by looking at its status in the queue
                    if(tasks[tid]['status'] == 'running'):
                        command=""
                        if(os.getenv('BSC_MACHINE') == 'mn4'):
                            command += "squeue -h -j"
                        if(os.getenv('BSC_MACHINE') == 'nord3'):
                            command += 'bjobs -noheader '
                        if(os.getenv('BSC_MACHINE') == None):
                            command += "error"
                        command+=tasks[tid]['remote_id']
                        process = subprocess.Popen(command, stdout=subprocess.PIPE,shell=True)
                        out, err = process.communicate()
                        if(out == ''):
                            if(os.getenv('BSC_MACHINE') == 'nord3'):
                                if('timer' in tasks[tid]):
                                    if(tasks[tid]['timer']> 10):
                                        tasks[tid]['status']="dead"
                                        logging.warning("task "+tasks[tid]['remote_id']+" not running")
                                    else:
                                        tasks[tid]['timer']=tasks[tid]['timer']+1
                                        print( tasks[tid]['timer'])
                                else:
                                    print("setup timer for job"+tasks[tid]['remote_id'])
                                    tasks[tid]['timer']=1
                            else:
                                tasks[tid]['status']="dead"
                                logging.warning("task "+tasks[tid]['remote_id']+" not running")

                    ##in every other case it means that the task ended so we should move on and start a new one
                    if(tasks[tid]['status'] != 'running' and tasks[tid]['status'] != 'hold'):
                            dead+=1

            ##update the pool of particule given their score if the experiment has finished
            tmp_keys=list(tmp_pdict.keys())
            for t in tmp_keys:
                tmp_exp=tmp_pdict[t]
                tmp_exp.gatherScore()
                if(tmp_exp.score>0):
                    if(tmp_exp.score >= epsilon):
                        tmp_exp.remove()
                        tmp_pdict.pop(t,None)
                    else:
                        if(len(pdict)<numParticule):
                            with open("tmp_res"+str(epsilon)+".csv",'a') as tmp_out:
                                tmp_out.write(tmp_exp.getId()+","+str(tmp_exp.score)+"\n")
                                tmp_out.close()
                            pdict[tmp_exp.getId()]=tmp_exp.score
                            newpool[tmp_exp.getId()]=tmp_exp
                        tmp_pdict.pop(t,None)

            #(the pool is empty ) all simulation finished and we have not yet enough particle
            #we regenerate a `numproc` number of experiments with paramter drawn from the original pool
            #this may be source of pb to check
            #if(len(pdict) < numParticule and dead == len(tasks)): 
            if(len(pdict) < numParticule and (dead == len(tasks) and len(tmp_pdict) <= 0)): 
                logging.info("regenerate new taskfiles")
                ###re-initialize pool
                tmp_pdict=renewPool(numproc,pref,oldpool)
                if(isNeedLauncher):
                    writeNupdate(tmp_pdict)
                ##findFileneNameAndUpdateCounter
                #Launch remaining tasks
        writeParticules(pdict,epsilon,"result_"+str(epsilon)+".csv")
        logging.info('send cancel signal to remaining tasks')
        if(isNeedLauncher):
            for tid,tproc in tasks.items():
                if(tasks[tid]['status'] == 'running'):
                    command=""
                    if(os.getenv('BSC_MACHINE') == 'mn4'):
                        command="scancel "+tasks[tid]['remote_id']
                    if(os.getenv('BSC_MACHINE') == 'nord3'):
                        command="bkill "+tasks[tid]['remote_id']
                    process = subprocess.Popen(command, stdout=subprocess.PIPE,shell=True)
                    out, err = process.communicate()
                    logging.info('force: '+tasks[tid]['remote_id']+" to stop. ")
        logging.info('ABC done for epsilon='+str(epsilon))
        pref="eps_"+str(epsilon) #this prefix is mainly use to store the data


        new_raw=rawMatricesFromPool(newpool)



        sigma=2 * weighted_cov(oldpool["thetas"],oldpool["ws"])
        new_raw["sigma"]=sigma
        new_raw["ws"]=[]
        for exp in newpool.values():
            theta=exp.params
            kernel = stats.multivariate_normal(theta, sigma, allow_singular=True).pdf
            new_raw["ws"].append(priors(theta) / np.sum(oldpool["ws"] * kernel(oldpool["thetas"])) )
        new_raw["ws"]=np.asarray(new_raw["ws"])
        #new_raw["ws"]=np.ones(len(oldpool["thetas"])) / len(oldpool["thetas"])
        
        oldpool=new_raw

        tmp_pdict=renewPool(numParticule,pref,oldpool)

        pdict={}     #list of score for each exp
        newpool={}     #list of score for each exp
        tasks={} #list of taskfiles that have to be send to mn, it allows to separate the experiments in different sbatch given some conditions

