from bs4 import BeautifulSoup as bs
import os,time
#index of the different parameters
indices= {  "mu"            : 0, 
            "nstep"        : 1,
            "cstep"         : 2}

##Check consistency of paramter
##generate the folders and files for the xp
class Experiment: 
    def __init__(self, expId, params,binpath,outpath):
        self.consistence=True
        self.params = params
        #self.expId = "_".join([str(int(self.params[indices['ngoods']])),str(int(self.params[indices['nag_good']])),str(self.params[indices['market_size']]),str(int(self.params[indices['cstep']])),str(self.params[indices['mu']])])
        self.expId = "_".join([str(int(self.params[indices['nstep']])),str(int(self.params[indices['cstep']])),str(self.params[indices['mu']])])
        self.binpath=binpath #binpath is the path where the executable & generic config ifle are stored 
        self.outpath=outpath

        #for key in indices.keys():
        #    print(key, ": ", self.params[indices[key]])
        # priors
        #print("prepare config file folder")

        if((int(self.params[indices['cstep']]) < 1 ) or  #No experiments if no cultural step
           #(int(self.params[indices['nag_good']]) < 1) or  #if num <2 
           #(int(self.params[indices['ngoods']]) < 2 ) or #No exchange possible if we don't have at least 2 goods
           (self.params[indices['mu']] <= 0 ) or #No meaning if mutation rate <0 or >1
           (self.params[indices['mu']] > 1 ) #or 
           #(self.params[indices['market_size']] > 1 ) or  #no need to explore more than 100% of the market
           #(self.params[indices['market_size']] <= 0 ) #agent has to visit the market to exchange stuff
          ):
            #print("baddd",params)
            self.consistence=False

        soup = bs(open(self.binpath+"/config.xml"),'xml') #read a generic config file ##need lxml installed
        
        self.kind=str(int(round(params[1]/1000)*1000))


        ##TODO .updateConfig()
        ##change the different value in the XML file with the parameters (thetas) of this experiments (particle)

        #soup.goods['num']=str(int(self.params[indices['ngoods']]))
        #soup.numAgents['value']=str(int(self.params[indices['ngoods']])*int(self.params[indices['nag_good']]))
        #soup.market['size']=str(self.params[indices['market_size']])
        soup.culture['step']=str(int(self.params[indices['cstep']]))
        soup.culture['mutation']=str(self.params[indices['mu']])
        soup.numSteps['value']=int(self.params[indices['nstep']])
        #soup.numSteps['value']=str(int(self.params[indices['cstep']])*3*100)
        soup.numSteps['serializeResolution']=int(soup.numSteps['value'])


        #TODO .createFolder()
        #create a directory to run experiment associated to this particle
        self.particleDirectory=os.path.join(self.outpath,self.expId)
        

        #print("config_"+str(self.expId)+".xml")
        if (not os.path.isdir(self.particleDirectory)) and self.consistence:
            os.mkdir(self.particleDirectory) #create folder for the exp
            os.mkdir(os.path.join(self.particleDirectory,"logs"))
            os.mkdir(os.path.join(self.particleDirectory,"data"))
            os.symlink(self.binpath+"/province",self.particleDirectory+ "/province") 
            os.symlink(self.binpath+"/AnalyseTools/analysis",self.particleDirectory+ "/analysis") 

            with open(self.particleDirectory+"/config.xml","a") as out:
                out.write(soup.prettify())
                out.close()
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
    def getScore(self):
        file_score=os.path.join(self.particleDirectory,"score.txt")
        time.sleep(.01)
        last_score=-1
        try:
            with open(file_score,"r") as score:
                    last_score=int(score.readline().strip())
        except IOError:
            last_score=-1
            #print(self.expId+" still running")
        return(last_score)

    def __str__(self):
        result = 'experiment: '+str(self.expId)#+' alpha: '+str('%.2f')%self.alpha+' beta: '+str('%.2f')%self.beta+' harbour bonus: '+str('%.2f')%self.harbourBonus
        return result

    #generate a string that countain the command that should be run on marenostrum
    def generateTask(experiment):
        #print("run pandora")
        bashCommand = 'cd '+experiment.particleDirectory + ' && ./province && ./analysis ' +' && cd -- &&'
        #output, error = process.communicate()
        bashCommand += 'bash ./extractlast.sh '+os.path.join(experiment.particleDirectory,'agents.csv &&')
        #output, error = process.communicate()
        bashCommand += 'rm -rf '+os.path.join(experiment.particleDirectory,"data") + ' '+os.path.join(experiment.particleDirectory,"logs")+ ' '+os.path.join(experiment.particleDirectory,"*.gdf \n")
        return bashCommand
        
    

