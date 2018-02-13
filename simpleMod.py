import os,time,sys,logging
import numpy as np

sep=","

order = 'mu'+sep+'sd'+sep+'n'

def genData():
    return np.random.normal(10,2,60)

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
        if(params[2] <= 10. or params[1] <= 0.0):
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
        result = 'experiment: '+str(self.expId)+' of kind:'+self.kind
        return result

    ####methods called by launcher

    #generate a string that countain the command that should be run on marenostrum
    def generateTask(self):
        print(self.params)
        return(np.random.normal(self.params[0],self.params[1],int(self.params[2])))

        
    #remove the entire folder of the particul
    def remove(self):
        return()

    #clean useless folder 
    def clean(self):
        return()

    #move the particule forlder
    def softRemove(self):
        return()

