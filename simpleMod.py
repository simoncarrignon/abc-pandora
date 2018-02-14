import os,time,sys,logging
import numpy as np
import warnings
warnings.filterwarnings('error')

sep=","

order = 'mu'+sep+'sd'+sep+'frac'

def genData():
    means=[]
    stds=[]
    pop=20.0
    sd=8
    mean=40
    fact=.05
    for time in range(20):
        dist=np.random.normal(mean,sd,int(pop))
        pop=pop+fact*pop
        print(pop)
        means.append(dist.mean())
        stds.append(dist.std())
    return(np.asarray((np.asarray(means),np.asarray(stds))))

##Check consistency of paramter
##generate the folders and files for the xp
class Experiment: 
    """
    Experiment
    
    :param params: scalar or array  parameters
    :param binpath: the path wher eis stored config file and executable
    :param outpath: path where will be stored expe config and outputfiles
    """
    
    def __init__(self, params,data):
        self.consistence=True
        self.params = params
        self.expId = "_".join(map(str,params))
        self.score=-1
        self.data=data
        self.kind=str("a")
        self.consistence=False
        if(params[1] <= 0.0 or params[0] <= 0.0):
            self.consistence=False
        else:
            self.consistence=True


    def getKind(self):
        return(self.kind)

    def getId(self):
        return(self.expId)

    #check if the score exist and return it, fi not return -1
    def gatherScore(self):
        return(self.generateTask())

    #check if the score exist and return it, fi not return -1
    def getScore(self):
        return(self.score)

    def __str__(self):
        result = 'experiment: '+str(self.expId)
        return result

    ####methods called by launcher

    #generate a string that countain the command that should be run on marenostrum
    def generateTask(self):
        means=[]
        stds=[]
        pop=20
        sd=self.params[1]
        mean=self.params[0]
        fact=self.params[2]
        for time in range(20):
            dist=np.random.normal(mean,sd,int(pop))
            means.append(dist.mean())
            stds.append(dist.std())
            pop=pop+fact*pop
            if(pop<1):pop=1
        return(np.asarray((np.asarray(means),np.asarray(stds))))

        
    #remove the entire folder of the particul
    def remove(self):
        logging.info("useless"+str(self)+",score:"+str(self.score))
        return()

    #clean useless folder 
    def clean(self):
        return()

    #move the particule forlder
    def softRemove(self):
        return()

