# PANDORA ABC
## Intro
Simple readme to do ABC on various architectures (at least MN4,tentative of Nord3 too) with ceeculture module of pandora. 


## Dependencies

### Internal Tools

* Obviously you need [pandora]() and [ceeculture]()
* you will need the script that compute the score of the experience:
	* for this version of ceeculture we utilise a Rscript include in ceeculture
	```bash
	  ln -s ~/ceeculture/AnalyseTools/computeScore.R .
	```
* we dont need nothing else as greasy works also very well with SLURM, so we dont need anymore `mn4_manual_scheduling`

```bash
ln -s ~/mn_tools/mn4_manual_scheduling.sh .
```

### Python


#### install manually python libs:
I will leave this here just in case, with the original exemple of beautifulsoup __but__ this package is not used anymore.


From source [here](http://bazaar.launchpad.net/~leonardr/beautifulsoup/bs4/changes)

you need to download a tarball from exemple [here]( http://bazaar.launchpad.net/~leonardr/beautifulsoup/bs4/revision/449?start_revid=449) and copy in marenostrum untar and go to the folder

befor edoing the install you will have to create a folder for manuall python installa nd add the folder to your PYTHONPATH 

```
mkdir  ${HOME}/python_libs
export PYTHONPATH=${HOME}/python_libs/lib/python:PYTHONPATH
```

then you can do
```bash
tar xzvf  tarbal.tgz
cd bs4
python setup.py install --home ${HOME}/python_libs
```
### on nord 3
If some python packages are missing install them same way as before

## usage exemple

So more or less you have everything. Obviously that wont work yet but let's imagine it will, we will run the experiments in a new folder. Dont forget that **ABC needs lot of space** so it may be wise to use a place like SCRATCH for your experiments (if your in amrenotrum, if not do what you want, that your hard drive)

let's do a new folder:

```bash 
mkdir $SCRATCH/MAGA #Make ABC Great Again
cd $SCRATCH/MAGA
```

If you haven't done it yet you can git clone the tools here:

```bash
git clone https://framagit.org/sc/abc-pandora/
cd abc-pandora
git checkout mn-dev #We suppose you are on marenostrum
cd ..
ln -s abc-pandora/* .
```

if you have installed everything already somewhere else :

```bash
ln -s ${HOME}/pandora_abc/* .
```

We mostly ready, in theory you are almost able to run  `./manual_abc.py`:
If you are in marenostrum don't run it in the login{1-3} node as you will be quickly killed. You can ask for a node or run a interactive job:

```bash
salloc -p interactive  
python ./manual_abc.py numParticule numpart  numproc_node epsilon
```

where 
* `numParticule`: total number of  particule (aka Thetas, aka set of parameter) that you to draw your distribution (the bigger, the better)
* `numpart`: number of particules we will generate and check at the same 
* `numproc_node`: number of parallele task we run in one marenostrum node  (should be < numpart as the idea is , from the numpart generated by the python script we split and  run them in separated nodes)
* `epsilon`: the maximum score we accept (o minimum) for the particule. _ie_ all particule should have a score < epsilon


Now that I realized that `greasy` was available you can just use the `2mn4.sh` script in the folder and do:

```bash
sbatch  2mn4.sh numParticule numpart  numproc_node epsilon
```

## Test the results

Once you are satisfied with a list of particle with `epsilon < N`, you can rerun more experiments using thos thetas. To do so use the script `rerun.py`

```bash
python rerun.py numParticule numpart_pernode pref epsilon
```

this will create `numParticule/numpart_pernode` taskfiles and a folder `pref-rerun` with all the experiments listed in the taskfiles. 

You can then simply send the files to marenostrum via greasy using rerun.job-mn4
