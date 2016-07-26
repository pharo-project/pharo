Pharo Process Description. This comment contains: 
	- some points on infrastructure
	- some scripts description 	
	- main behavior of the scriptLoader.
	
General points on infrastructure 
---------------------------------------------------------------------------------------------------------------------------------
An enhancement
	- should be added to the bug tracker https://pharo.fogbugz.com
	- announced to the mailing-list
	- asked for feedback 
	- results should be added to the BT entry

FixedToInclude tag means ready for integration.
FixToreview means that people should have a look and give feedback (often just need to FixToInclude)

A bug detected and discussed via the mailing-list should be added to the bug tracker. 
When a fix is provided, it should be either posted on the bug tracker as a slice in the PharoInbox - (a slice is an emtpy package that has as requirement other package composing the fix).

- Repositories
---------------------
For each Pharo version, we have three projects:
	Pharo
	PharoInbox
	(PharoTreatedInbox - was used in Pharo20 and we hope to get it back)
	
A fix goes either from inbox to treatedInbox or to Pharo.
If a fix does not work it is moved to the TreatedInbox.
If a fix works it is integrated as follow - it will be moved from the Inbox to the TreatedInbox
and integrated and published in the Pharo project


It is now a bit old but to help browsing multiple repositories use the following expression: ScriptLoader new addExtraRepositories

	self addRepository39ToAllPackages.
	self addRepository310ToAllPackages.
	self addRepositoryTaskForcesToAllPackages.
	self addRepositorySqueakTrunkToAllPackages.
	self addRepositoryMCToAllPackages.
	self addRepositoryTreatedToAllPackages.

System Scripts
-------------------------

Before starting and you need to download (only once) the pharo-updates folder from git hub:

	git clone https://github.com/pharo-project/pharo-update.git
	cd pharo-update
	git checkout 40

to work on the right branch



Typically before any integration, execute the script prepare.sh 
----------------------------------------------------------------------------
#! /bin/bash

version=3.0
working_dir=current
previous_dir=previous
zip_name=latest.zip

if [ -e $zip_name ]; then
        rm $zip_name
fi
wget --no-check-certificate http://files.pharo.org/image/30/$zip_name
rm -Rf $previous_dir
mv $working_dir $previous_dir
unzip -d $working_dir $zip_name  
mv $working_dir/Pharo*.image $working_dir/Pharo.image
mv $working_dir/Pharo*.changes $working_dir/Pharo.changes
cp PharoPass.txt $working_dir/PharoPass.txt
./scripts/getupdateslist.sh
pharo $working_dir/Pharo.image

the script getupdateslist.sh is defined as follows: 
----------------------------------------------------------------------------
#!/bin/bash

flatten_version=30
updates_dir=./pharo-update
working_dir=current
current_dir=`pwd`

cd $updates_dir
git pull
cd $current_dir
cp $updates_dir/updates$flatten_version.staged $working_dir


Later when you will be done and you will kick the integration server you will need the following publish script.

The publish.sh script is defined as follows:

#! /bin/bash

version=3.0
updates_dir=./pharo-update
current_dir=current
previous_dir=previous

cp $current_dir/*.cs $updates_dir
cp $current_dir/*.staged $updates_dir
./scripts/publish.sh





4 Steps of the integration process
------------------------------------------------------
Precondition: you have run the ./prepare.sh scripts and before that one you should do a git clone of the pharo-update repository.

The integration works in 4 main steps which can be steered by the following expression
ScriptLoader releaseMenu

or via the menu that can be shown/hidden using 
	ScriptLoader showIntegrationMenu
	ScriptLoader hideIntegrationMenu



---------------------------------------------------------------------------------------------------------------------------------
1.) 'Prepare new update' :  Start up a recent and clean image
	ScriptLoader new prepareNewUpdate
	
	This step will 
		- load the latest updates
		
		- load the latest version of the ScriptLoader package from the Pharo repository.
		Indeed when we work on improving the ScriptLoader it may not be published in the update stream. 
		New versions can be available on the server but not part of the latest updates.
		Therefore the process always loads the latest version of scriptloader from the pharo repository.
		This point is important because when an integration failed, you have to check and optionally 
		remove the scriptloader package from the pharo repository or republished a previous version with a more recent 		number to override the last one.
		
		- check that the update.list (which contains the cs to load the packages) is in sync
		with the image current version. The update was done automatically by the prepare bash script (which pulls from git).
		
		- snapshot the package version to detect dirty or changed but non dirty packages.

2.) Apply changes

	Once you are done and select the second menu item or  
	
		ScriptLoader new doneApplyingChanges
	
	This step will 
		- create an update method with can trigger the load of the packages and some pre/post actions
		- create a script method with describes all the package versions and it used by the update methods
		- save all the packages that are different (except some filtered packages)	
		into a local folder named 'package-to-be-tested'.
		ScriptLoader, SLICE*.... and a couple of other packages are not considered to be included in the script method
			(check method packagesNotToSavePatternNames)
		

3.) Verify changes
	==> in a ***new*** image (in the current folder) execute:
	ScriptLoader new verifyNewUpdate

	This step will 
		- load in any order (so may break) the packages previously saved
		in the 'package-to-be-tested' folder.
		- this step is important because you may get simple changes with unexpected side effects and that 
		may break the load.
		

4.) If there are problems go to 2.) to fix them, else you can commit the changes.

	ScriptLoader new publishChanges

	This step will
		- generate a new cs file whose purpose is to load the given version of the scriptloader and trigger the 
		correct update method.
		- add the name of the cs file to the end of the updates.staged file local to the disc
		- copy all the package from the local directory to the Pharo
		
	At the current time (August 2013), you will get the contents of the mail that you can send to the list. 
	Now you will have to wait before sending it because you need to kick in the ci integration process. 
			Execute the publish.sh bash script to publish the cs and the staged file in git.
	
	Then go to the https://ci.inria.fr/pharo/? page.
	You will see that the ci will validate your integration automatically.
		First it will load the code (strangely named: Step-1-Tracker) 
		Second it will validate it: Step-2-Validation 
		Third it will release it: Step-3-Release 
		Fourth it will publish it: Step-4-Publish 
	
	As a result the pharo-update and a new image will be published.
	You can then send the mail of the new update.
		
		


5) in case of problems.
If a problem arrives during the step 4, your update.staged file and your image may not in sync! 
Do not stress! Normally it should be easy to fix.
		 
If the ci integration failed previously: 
		- 1. Load the previous version of the Scriptloader, modify it to get the package dirty and republish it. It will get a newer number and the setting up of the integration will load it instead of a version of ScriptLoader containing an integration that did not finish.
		-2 just throw away your current directory. Do not worry about the fact that the changeset has been added to git. When you will integrate, the process will create a file with the same number and erase the committed one. 

Note that if you have to rollback the created image you have to access the file repository to remove the latest image.
		The file server is available at
		 files.pharo.org
			Server:		193.51.235.3
			Address:	193.51.235.3#53
		It may be possible that you do not access and need to get your ssh files there first. 




Other notes.
--------------------

CurrentMajorVersionNumber should contains a string '1.0', '1.1'....
This string will determine on which folder on the server the updates.list should be loaded.
	i.e., updates/pharo1.0, updates/pharo1.1 ....

--- Not to forget ---
To change the release stream
add a new method for each releaseStream
	ScriptLoader toPharoOne
	ScriptLoader toPharoOneDotOne
