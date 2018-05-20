import os,time,sys,logging
import numpy as np
import random
from apemcc.data.ceramic import *
from apemcc.model.apemcc import CCSimu


##wraper clas of CCSimu to be called by the ABC

sep=","

order = 'max_time'+sep+'mu'+sep+'copy'+sep+'alpha'+sep+"str1"+sep+"str2"+sep+"str3" + sep+ "str4"

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
        #CCSimu param: (self,n_ws,max_time,pref,model,p_mu,p_copy,b_dist,init)
        ##here check the parameters but also 
        strmu=params[4:8]
        if( (params[1] < 0.0 or params[1] > 1.0) or params[0] <= 0.0 or (params[2] < 0.0 or params[2] > 1.0) or params[3] > 1.0 or params[3] < -1.0 or (strmu < 0).any()):
            self.consistence=False
        else:
            if (not os.path.isdir(pref)):
                os.mkdir(pref)
            self.consistence=True
            print(params[4:8])
            self.ccsimu = CCSimu(5,int(params[0]),self.particleDirectory,-1,params[1],params[2],params[3],"file",realdist,outputfile=False,mu_str=strmu)

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
        ameans=[]
        for ws in self.ccsimu.world:
            tmean=0
            s=samplesize[ws.id]
            tot_prod=len(ws.production[ws.all_measures.keys()[1]])
            if(s>tot_prod):s=tot_prod
            sel=random.sample(range(0,tot_prod), s)
            for measure in ws.all_measures:
                allprod=ws.production[measure]
                selprod=[allprod[i] for i in sel]
                tmean=np.mean(selprod)
                diff=abs(float(tmean)-float(allmeans[ws.id][measure]))
                ameans.append(diff)
        self.score=np.mean(ameans)
        print("score="+str(self.score))
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


