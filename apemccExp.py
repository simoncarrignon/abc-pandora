import os,time,sys,logging
import numpy as np
from apemcc import CCSimu
from apemcc import realmeans

##wraper clas of CCSimu to be called by the ABC

sep=","

order = 'max_time'+sep+'mu'+sep+'copy'+sep+'alpha'

##Check consistency of paramter
##generate the folders and files for the xp
class Experiment: 
    """
    Experiment
    
    :param params: scalar or array  parameters
    :param binpath: the path wher eis stored config file and executable
    :param outpath: path where will be stored expe config and outputfiles
    """
    
    def __init__(self, params,pref):
        self.consistence=True
        self.params = params
        self.expId = "_".join(map(str,params))
        self.score=-1
        self.particleDirectory=os.path.join(str(pref),self.expId)
        self.kind=str("a")
        self.consistence=False
    #def __init__(self,n_ws,max_time,pref,model,p_mu,p_copy,b_dist,init):
        ##here check the parameters but also 
        if(params[1] <= 0.0 or params[0] <= 0.0 or params[2] < 0.0 or params[3] > 1.0 or params[3] < -1.0):
            self.consistence=False
        else:
            self.consistence=True
        if (not os.path.isdir(pref)) and self.consistence:
            os.mkdir(pref)
        self.ccsimu = CCSimu(5,int(params[0]),self.particleDirectory,-1,params[1],params[2],params[3],"file")

    def getKind(self):
        return(self.kind)

    def getId(self):
        return(self.expId)

    #check if the score exist and return it, fi not return -1
    def gatherScore(self):
        self.ccsimu.run()
        return(self.getScore())

    #check if the score exist and return it, fi not return -1
    def getScore(self):
        allmeans=[]
        for ws in self.ccsimu.world:
            tmean=np.mean(ws.production["exterior_diam"])
            allmeans.append(tmean-realmeans[ws.id])
        self.score=np.mean(allmeans)
        return(self.score)

    def __str__(self):
        result = 'experiment: '+str(self.expId)
        return result

    ####methods called by launcher

    #generate a string that countain the command that should be run on marenostrum
    #or directly run the task if the model is to be called directl
    def generateTask(self):
        self.ccsimu.run()       
        for w in self.ccsimu.world:
            print(w.id)
            print(realmeans[w.id])
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


