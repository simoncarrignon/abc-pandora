from bs4 import BeautifulSoup as bs
import os,time,sys,logging
from shutil import rmtree
import subprocess

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
        filename_score=os.path.join(self.particleDirectory,"score.txt")
        try:
            with open(filename_score,"r") as file_score:
		try:
                 	self.score=float(file_score.readline().strip())
		except :
            		logging.warning("score in a bad format")
		self.clean()
	
        except IOError:
            logging.debug(str(self)+" still loading")
            self.score=-1
    #check if the score exist and return it, fi not return -1
    def getScore(self):
        return(self.score)

    def __str__(self):
        result = 'experiment: '+str(self.expId)+' of kind:'+self.kind
        return result

    #generate a string that countain the command that should be run on marenostrum
    def generateTask(self):
	n_years=50
        #print("run pandora")
        bashCommand = 'cd '+self.particleDirectory + ' && ./province && ./analysis ' +' && cd - &&'
        bashCommand += '/apps/R/3.4.0/bin/Rscript --vanilla computeScore.R ' + self.particleDirectory + ' ' + str(n_years)+'\n'
        #bashCommand += 'rm -rf '+os.path.join(self.particleDirectory,"data") + ' '+os.path.join(self.particleDirectory,"logs")+ ' '+os.path.join(self.particleDirectory,"*.gdf \n")
        return bashCommand
        
    #remove the entire folder of the particul
    def remove(self):
        try:
            subprocess.Popen(["rm","-rf",self.particleDirectory])
            logging.info("rm:"+self.expId+",score was:"+str(self.score))
        except Exception as e:
            print(e)

    #clean useless folder 
    def clean(self):
        try:
            subprocess.Popen(["rm","-rf",os.path.join(self.particleDirectory,"logs")])
            subprocess.Popen(["rm","-rf",os.path.join(self.particleDirectory,"data")])
            logging.debug("cleaned "+self.expId+" logs and data")
        except Exception as e:
            print(e)

    #move the particule forlder
    def softRemove(self):
        try:
            rmtree(self.particleDirectory)
            logging.info("soft rm:"+self.expId+",score was:"+str(self.score))
        except Exception as e:
            print(e)

