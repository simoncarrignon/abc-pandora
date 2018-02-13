import os,time,sys,logging
import random

##Check consistency of paramter
##generate the folders and files for the xp
class Experiment: 
    """
    Experiment
    
    :param params: scalar or array  parameters
    :param binpath: the path wher eis stored config file and executable
    :param outpath: path where will be stored expe config and outputfiles
    """
    
    def __init__(self, params,prefId=""):
        binpath=""

        self.consistence=True
        self.params = params
        self.expId = "_".join(map(str,param))
        self.score=-1
        
        self.kind=str("a")


    def getKind(self):
        return(self.kind)

    def getId(self):
        return(self.expId)

    #check if the score exist and return it, fi not return -1
    def gatherScore(self):
        return(random.random())

    #check if the score exist and return it, fi not return -1
    def getScore(self):
        return(self.score)

    def __str__(self):
        result = 'experiment: '+str(self.expId)+' of kind:'+self.kind
        return result

    ####methods called by launcher

    #generate a string that countain the command that should be run on marenostrum
    def generateTask(self):
        
    #remove the entire folder of the particul
    def remove(self):

    #clean useless folder 
    def clean(self):

    #move the particule forlder
    def softRemove(self):

