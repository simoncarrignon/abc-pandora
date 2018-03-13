import xml.etree.ElementTree as xtree
import os,time,sys,logging
from shutil import rmtree
import subprocess

#index of the different parameters
indices= {  "mu"            : 0, 
            "mumax"        : 1,
            "copy"        : 2,
            "nstep"        : 3,
            "cstep"         : 4}
sep=","
order = 'nstep'+sep+'cstep'+sep+'mu'+sep+'mumax'+sep+'copy'


###XL PARSER TOOL##########
def parseConfig(filename):
    """
    parse
    :param filename: the name of a config file
    :return a dictionnary where all leaf of the tree are duct["leaf.tag"]=leaf.attribute
    """
    tree = xtree.parse(filename)
    parsedFile={}
    parse(tree.getroot(),parsedFile)
    return((parsedFile,tree))

def parse(root,res):
    """
    parse
    :param root: the root of a xml.etree.ElementTree
    :param res: an empty dictionnary where to store the tree
    """
    for child in root:
        if(len(child) == 0):
            res[child.tag]=child.attrib
        if(len(child) > 0):
            parse(child,res)
#############################



##Check consistency of parameter
##generate the folders and files for the xp
class Experiment: 
    """
    Experiment
    
    :param params: scalar or array  parameters
    :param binpath: the path wher eis stored config file and executable
    :param outpath: path where will be stored expe config and outputfiles
    """
    
    def __init__(self, params,outpath, prefId=""):
        binpath=""
        if(os.getenv('BSC_MACHINE')):
            binpath="/home/bsc21/bsc21394/ceeculture/"
        else:
            binpath="/home/scarrign/ceeculture/"

        self.consistence=True
        self.params = params
        self.expId = "_".join([str(int(self.params[indices['nstep']])),str(int(self.params[indices['cstep']])),str(self.params[indices['mu']]),str(self.params[indices['mumax']]),str(self.params[indices['copy']])])
        self.binpath=binpath #binpath is the path where the executable & generic config ifle are stored 
        self.outpath=outpath
        self.score=-1

        if((int(self.params[indices['cstep']]) < 1 ) or  #No experiments if no cultural step
           #(int(self.params[indices['nag_good']]) < 1) or  #if num <2 
           #(int(self.params[indices['ngoods']]) < 2 ) or #No exchange possible if we don't have at least 2 goods
           (self.params[indices['mumax']] <= 0 ) or #No meaning if mutation rate <0 or >1
           (self.params[indices['copy']] <= 0 ) or #No meaning if mutation rate <0 or >1
           (self.params[indices['nstep']]/(self.params[indices['cstep']]) < 30 ) or #not enough cultural step to extract meaningful information
           (self.params[indices['mu']] <= 0 ) or #No meaning if mutation rate <0 or >1
           (self.params[indices['mu']] > 1 ) #or 
           #(self.params[indices['market_size']] > 1 ) or  #no need to explore more than 100% of the market
           #(self.params[indices['market_size']] <= 0 ) #agent has to visit the market to exchange stuff
          ):
            self.consistence=False
            logging.warning( "unconsistent particle")  
        else:
            soup,tree = parseConfig(self.binpath+"/config.xml") #
            
            self.kind=str(int(round(params[indices['nstep']]/1000)*1000))


            ##TODO .updateConfig()
            ##change the different value in the XML file with the parameters (thetas) of this experiments (particle)

            soup["numAgents"]['value']=str(250)
            soup["culture"]['step']=str(int(self.params[indices['cstep']]))
            soup["culture"]['mutation']=str(self.params[indices['mu']])
            soup["culture"]['mumax']=str(self.params[indices['mumax']])
            soup["culture"]['copy']=str(self.params[indices['copy']])
            soup["numSteps"]['value']=str(int(self.params[indices['nstep']])*3)
            soup["numSteps"]['serializeResolution']=str(3*int(self.params[indices['cstep']]))
            soup["events"]['rate']=str(int(self.params[indices['nstep']])/(4*int(self.params[indices['cstep']]) ))


            #TODO .createFolder()
            #create a directory to run experiment associated to this particle
            self.particleDirectory=os.path.join(self.outpath,self.expId)
            

            #print("config_"+str(self.expId)+".xml")
            if (not os.path.isdir(self.particleDirectory)) and self.consistence:
                os.makedirs(self.particleDirectory) #create folder for the exp
                os.mkdir(os.path.join(self.particleDirectory,"logs"))
                os.mkdir(os.path.join(self.particleDirectory,"data"))
                if(os.getenv('BSC_MACHINE') == 'mn4'):
                    os.symlink(self.binpath+"/build/ceec",self.particleDirectory+ "/province") 
                    os.symlink(self.binpath+"/AnalyseTools/build/analysis",self.particleDirectory+ "/analysis") 
                if(os.getenv('BSC_MACHINE') == 'nord3'):
                    os.symlink(self.binpath+"/province",self.particleDirectory+ "/province") 
                    os.symlink(self.binpath+"/AnalyseTools/analysis",self.particleDirectory+ "/analysis") 

                tree.write(self.particleDirectory+"/config.xml")
                #with open(self.particleDirectory+"/config.xml","a") as out:
                #    out.write(soup.prettify())
                #    out.close()
            else:
                if (os.path.isdir(self.particleDirectory)):  
                    logging.warning( "particle already tested")  
                else:
                    logging.warning( "unconsistent particle")  
                self.consistence=False

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
        pattern="dis" 
        numsite=60
        diffstr="enriscore" 
        #print("run pandora")
        bashCommand = 'cd '+self.particleDirectory + ' && ./province && ./analysis ' +' && cd - &&'
        if(os.getenv('BSC_MACHINE') == 'mn4'):
            bashCommand += '/apps/R/3.4.0/bin/Rscript --vanilla computeScore.R ' + self.particleDirectory + ' ' + str(n_years)+'\n'
        if(os.getenv('BSC_MACHINE') == 'nord3'):
            bashCommand += '/apps/R/3.2.2/bin/Rscript --vanilla computeScore.R ' + self.particleDirectory + ' ' + str(n_years)+' ' + str(diffstr)+' ' + str(pattern)+' ' + str(numsite)+'\n'
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
