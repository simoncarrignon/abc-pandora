#!/usr/bin/python3
#TODO : a Class Launcher and a Class Tasks (a class task being a set of experiments , a class launcher an abstract class implemented for each architecture (MN[3-4]/LAPTOP/HSELDON...)

import csv, math, sys, argparse, random,os,errno,re
import numpy as np
import logging
import time
import subprocess
import pickle
from scipy import stats
from sampler import TophatPrior
from sampler import weighted_cov
from ceecexp import Experiment
from ceecexp import order
from ceecexp import indices


#return true if the application reach a limit too close to a total time ttime
def checkTime(start_time,ttime,limit):
    return(ttime - (time.time()-start_time) < limit) 


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
            countExpKind[kind]=1
            countFileKind[kind]=1
        else:
            countExpKind[kind]=countExpKind[kind]+1 #increase number of expe of this kind
            if(countExpKind[kind] > (numproc_node)): #if number of expe is too high, increase number of file 
                #TODO here should launch the file already full fullfillfillfull
                countFileKind[kind]=countFileKind[kind]+1
                countExpKind[kind]=1

        if (not os.path.isdir("taskfiles")):
            os.makedirs("taskfiles") #create folder for the taskfiles

        taskid=kind+"_"+str(countFileKind[kind])
        taskfilename=taskid+"-"+jobid+".task"
        taskfilename=os.path.join("taskfiles",taskfilename)

        while( os.path.isfile(taskfilename) and not(taskid in tasks.keys())):
            logging.debug(taskfilename+" already exist from previous task so we have to use another one")
            countFileKind[kind]=countFileKind[kind]+1
            countExpKind[kind]=1
            taskid=kind+"_"+str(countFileKind[kind])
            taskfilename=taskid+"-"+jobid+".task"
            taskfilename=os.path.join("taskfiles",taskfilename)
        with open(taskfilename,'a') as tskf:
            tskf.write(task)

    #TODO task should be handled as object
        if( not( taskid in tasks.keys())): #check if this kind is already recorded
            tasks[taskid]={}
            tasks[taskid]['filename']=taskfilename
            tasks[taskid]['status']='hold'
            tasks[taskid]['kind']=kind

    logging.debug(tasks)

###launch batch of experiments given the machine used
#TODO a real class "launcher" that can abstract that for the ABC
def launchExpe(taskfile,stime):
    t=stime #time in minutes (ie for 1h30: t=150)
    h=int(t/60)
    m=int(t-60*h)
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
#* `numpart`: number of particules we will generate 
#* `numproc_node`: number of job per task (ie number of node) 
#* `pref`: prefix for this experiments
#* `epsilon`: the file with the selected thetas
#



if __name__ == '__main__' :
    pdict={}     #list of score for each exp
    newpool={}     #list of score for each exp
    countExpKind={} #number of experiment for each different "kind" 
    countFileKind={} #number of tasksfile for each different "kind" 
    tasks={} #list of taskfiles that have to be send to mn, it allows to separate the experiments in different sbatch given some conditions
    tmp_pdict={} #temporary pool of particules
    numParticule=int(sys.argv[1]) #This is the total number of  particule (aka Thetas, aka set of parameter) that we want
    numproc=int(sys.argv[2]) #this is the number of parallele task we will try
    numproc_node=int(sys.argv[3]) #this is the number of parallele task we will try
    #epsilon=float(sys.argv[4])  #the maximum score we accept (o minimum)#changing the last argument for the time asked for subjobs (temporary, should be computed given cstep and nstep)
    #stime=float(sys.argv[4])  #changing the last argument for the time asked for subjobs (temporary, should be computed given cstep and nstep)
    pref=str(sys.argv[4])  #changing the last argument for the time asked for subjobs (temporary, should be computed given cstep and nstep)
    epsilon=str(sys.argv[5])  #the file storing the epsilon to test
    pref=pref+"-"+re.sub("result_","",epsilon)[0:8] #assuming the epsilon is of the form result_1.232342, we take the 8 first digit

    start_time=time.time()
    alltotest=[]
    with open(epsilon,'rb') as storedthetas:
        thetastotest=csv.reader(storedthetas, quoting=csv.QUOTE_NONNUMERIC) #unquoted text as float
        try:
            next(thetastotest, None) #skip the header
        except:
            print("skip header")
        for row in thetastotest:
            alltotest.append(row)
    alltotest=np.asarray(alltotest)

    task=0
    for e in range(numParticule):

        idx=random.randint(0,len(alltotest)-1)
        params=alltotest[idx]
        params=params[0:(len(params)-2)] #-2 to skip the score and the epsilon that are stored in the final CSV by manual_abc.py
        one=Experiment(params,"rerun-"+str(pref),rerun=1)
        if(e % numproc == 0 ):
            task+=1
        with open("rerun-"+pref+".task."+str(task),'a') as tskf:
            tskf.write(one.generateTask())


        #if(epsilon>1):
        #    round_epsilon=np.round(epsilon)
        #else:
        #    round_epsilon=np.round(epsilon,decimals=4)

        #pref="eps_"+str(round_epsilon) #this prefix is mainly use to store the data
        #logging.debug("restarting with eps: %f " % epsilon)
        #logging.debug("restarting with pref: %s " % pref)

        #tmpres="tmp_res"+str(round_epsilon)+".csv"
        #if(not (os.path.isfile(tmpres))):
        #    with open(tmpres,'w') as tmp_out:
        #        tmp_out.write("id,score\n")
        #        tmp_out.close()

        ####initialize pool
        #if( isNeedLauncher and not backup):
        #    writeNupdate(tmp_pdict)

        ###findFileneNameAndUpdateCounter
        ##Launch remaining tasks

        #oldlen=0
        #while(len(pdict) < numParticule):

        #    if(len(pdict)>oldlen): ##logging only if new particules found
        #        oldlen=len(pdict)
        #        logging.info(str(len(pdict))+ "/"+str(numParticule)+ " tot")

        #    dead=0

        #    if(isNeedLauncher):
        #    ##################################################:LAUNCHING (slurm or bsub given the machin)
        #        for tid,tproc in tasks.items():
        #            ##check status of the task
        #            #if on hold it means it has been created during previous loop and has to be launched
        #            if(tasks[tid]['status'] == 'hold'):
        #                launcher=launchExpe(tasks[tid]['filename'],stime)
        #                out, err = launcher.communicate()
        #                logging.info(out)
        #                try:
        #                    remote_id=""
        #                    if(os.getenv('BSC_MACHINE') == 'mn4'):
        #                        remote_id=re.search('Submitted batch job ([0-9]+)\n',out).group(1)
        #                    if(os.getenv('BSC_MACHINE') == 'nord3'):
        #                        remote_id=re.search('Job <([0-9]+)> is submitted',out).group(1)
        #                    tasks[tid]['status'] = 'running'
        #                    tasks[tid]['remote_id'] = remote_id
        #                    logging.debug("lauching"+str(tid)+" on "+remote_id)
        #                except:
        #                    logging.warning("Task ID not found")
        #                    tasks[tid]['status'] = ''
        #                    logging.warning('problem while launching the job')
        #                #if the task is running (meaning a greasy job has been launched) we check if the job is still running
        #                # by looking at its status in the queue
        #            if(tasks[tid]['status'] == 'running'):
        #                command=""
        #                if(os.getenv('BSC_MACHINE') == 'mn4'):
        #                    command += "squeue -h -j"
        #                if(os.getenv('BSC_MACHINE') == 'nord3'):
        #                    command += 'bjobs -noheader '
        #                if(os.getenv('BSC_MACHINE') == None):
        #                    command += "error"
        #                command+=tasks[tid]['remote_id']
        #                process = subprocess.Popen(command, stdout=subprocess.PIPE,shell=True)
        #                out, err = process.communicate()
        #                if(out == ''):
        #                    ##In nord3 there is some lag before that the job appears after bjobs command so we setup a timer
        #                    timer=50 
        #                    if(os.getenv('BSC_MACHINE') == 'nord3'):
        #                        if('timer' in tasks[tid]):
        #                            if(tasks[tid]['timer']> timer):
        #                                tasks[tid]['status']="dead"
        #                                dead+=1
        #                                logging.warning("task "+tasks[tid]['remote_id']+" not running")
        #                            else:
        #                                tasks[tid]['timer']=tasks[tid]['timer']+1
        #                                logging.warning("increasing "+tasks[tid]['remote_id']+" timer:"+str(tasks[tid]['timer'])+"/"+str(timer))
        #                        else:
        #                            logging.warning("setup timer for job "+tasks[tid]['remote_id'])
        #                            tasks[tid]['timer']=1
        #                    else:
        #                        tasks[tid]['status']="dead"
        #                        dead+=1
        #                        logging.warning("task "+tasks[tid]['remote_id']+" not running")
        #                else: #job has been found running on the remote machine so next time if it's not found anymore it means it'sdead:
        #                    if(os.getenv('BSC_MACHINE') == 'nord3'): ##not so easy as the jobs reamins in memory 
        #                        try:
        #                            stat=re.search(str(tasks[tid]['remote_id'])+'\s\w+\s([A-Z]+)\s.*',out).group(1)
        #                            if(stat=="DONE" or stat=="EXIT"):
        #                                tasks[tid]['status']="dead"
        #                                dead+=1 #this is not consistent with following if
        #                        except:
        #                            logging.warning("Uknown message"+out+"returned by bjobs")
        #                    else: #we are on mn4, the jobs is done and not tracket by bjobs anymore, the jobs has been sent but is not tracked yet by bjobs.
        #                        tasks[tid]['timer']=timer - 1



        #            ##in every other case it means that the task ended so we should move on and start a new one
        #            if(tasks[tid]['status'] != 'running' and tasks[tid]['status'] != 'hold'):
        #                    dead+=1

        #    ##update the pool of particule given their score if the experiment has finished
        #    tmp_keys=list(tmp_pdict.keys())
        #    for t in tmp_keys:
        #        tmp_exp=tmp_pdict[t]
        #        tmp_exp.gatherScore()
        #        if(tmp_exp.score>0):
        #            if(tmp_exp.score >= epsilon):
        #                tmp_exp.remove()
        #                tmp_pdict.pop(t,None)
        #            else:
        #                if(len(pdict)<numParticule):
        #                    with open(tmpres,'a') as tmp_out:
        #                        tmp_out.write(tmp_exp.getId()+","+str(tmp_exp.score)+"\n")
        #                        tmp_out.close()
        #                    pdict[tmp_exp.getId()]=tmp_exp.score
        #                    newpool[tmp_exp.getId()]=tmp_exp
        #                tmp_pdict.pop(t,None)
        #        if(not tmp_exp.consistence):
        #                tmp_pdict.pop(t,None)

        #    #(the pool is empty ) all simulation finished and we have not yet enough particle
        #    #we regenerate a `numproc` number of experiments with paramter drawn from the original pool
        #    #this may be source of pb to check
        #    #if(len(pdict) < numParticule and dead == len(tasks)): 
        #    logging.debug("lenpdict:"+str(len(pdict))+" len tmp_pdict:"+str(len(tmp_pdict))+" len tasks:"+ str(len(tasks))+" dead:"+str(dead))
        #    if((len(pdict) < numParticule and len(tmp_pdict) <= 0) or (len(pdict) < numParticule and len(tasks) == dead)): 
        #        if(len(tmp_pdict)>0):
        #            for i in tmp_pdict:
        #                logging.warning("those experiments should be destroyd")
        #        logging.info("regenerate new taskfiles")
        #        ###re-initialize pool
        #        tmp_pdict=renewPool(numproc,pref,oldpool)
        #        if(isNeedLauncher):
        #            writeNupdate(tmp_pdict)
        #        ##findFileneNameAndUpdateCounter
        #        #Launch remaining tasks
        #    if(checkTime(start_time,ttime*60,30) or (len(pdict) == numParticule) ):
        #            logging.info("log backup")
        #            pickle.dump(tmp_pdict,open(os.path.join(backup_fold,"tmp_pdict"),"w"))
        #            pickle.dump(pdict,open(os.path.join(backup_fold,"pdict"),"w"))
        #            pickle.dump(oldpool,open(os.path.join(backup_fold,"oldpool"),"w"))
        #            pickle.dump(epsilon,open(os.path.join(backup_fold,"epsilon"),"w"))
        #            pickle.dump(newpool,open(os.path.join(backup_fold,"newpool"),"w"))
        #            pickle.dump(pref,open(os.path.join(backup_fold,"pref"),"w"))
        #            pickle.dump(tasks,open(os.path.join(backup_fold,"tasks"),"w"))
        #            if(checkTime(start_time,ttime*60,30)):
        #                logging.info("safe shutdown")
        #                print("shutdown hasta luego")
        #                exit(0)

        ##print(tmp_pdict)

        #writeParticules(pdict,epsilon,"result_"+str(epsilon)+".csv")
        #logging.info('send cancel signal to remaining tasks')
        #if(isNeedLauncher):
        #    for tid,tproc in tasks.items():
        #        if(tasks[tid]['status'] == 'running'):
        #            command=""
        #            if(os.getenv('BSC_MACHINE') == 'mn4'):
        #                command="scancel "+tasks[tid]['remote_id']
        #            if(os.getenv('BSC_MACHINE') == 'nord3'):
        #                command="bkill "+tasks[tid]['remote_id']
        #            process = subprocess.Popen(command, stdout=subprocess.PIPE,shell=True)
        #            out, err = process.communicate()
        #            logging.info('force: '+tasks[tid]['remote_id']+" to stop. ")
        #logging.info('ABC done for epsilon='+str(epsilon))


        #new_raw=rawMatricesFromPool(newpool)



        #sigma=2 * weighted_cov(oldpool["thetas"],oldpool["ws"])
        #new_raw["sigma"]=sigma
        #new_raw["ws"]=[]
        #for exp in newpool.values():
        #    theta=exp.params
        #    kernel = stats.multivariate_normal(theta, sigma, allow_singular=True).pdf
        #    new_raw["ws"].append(priors(theta) / np.sum(oldpool["ws"] * kernel(oldpool["thetas"])) )

        #new_raw["ws"]=np.asarray(new_raw["ws"]/np.sum(new_raw["ws"]))
        ##new_raw["ws"]=np.ones(len(oldpool["thetas"])) / len(oldpool["thetas"])
        #
        #oldpool=new_raw

        #tmp_pdict=renewPool(numParticule,pref,oldpool)
        ##print(tmp_pdict)

        #pdict={}     #list of score for each exp
        #newpool={}     #list of score for each exp
        #tasks={} #list of taskfiles that have to be send to mn, it allows to separate the experiments in different sbatch given some conditions

