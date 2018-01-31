# PANDORA ABC
## Intro
Simple readme to do ABC on MN4 with ceeculture module of pandora

## Dependencies

### Internal Tools

* OBviously you need [pandora]() and [ceeculture]()
* you will need the R script to compute the score of the experience
* For marenostrum you will need the script to launch the experiments which is in `mn_tools`

```bash
ln -s ~/mn_tools/mn4_manual_scheduling.sh ."
```

## Python
First fthing is to install the python dependencies which should be only:  beautiful soup, a conveniant w2ay to read the config.xml files (though obviouslty I should be using a python xml reader but ...


### install beautifulsoup 


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

YOur not done yet as bs4 need lxml to read the config file
	
### instal LXML  from source
if you did good previously, no need for much, you will find source of lxml on github here:
git clone https://github.com/lxml/lxml.git

then copy them on MN4 and:

```bash
cd lxml
python setup.py install --home ${HOME}/python_libs
```

