menu integration actions
prepareNewUpdate
	self launchUpdate.
	self loadLatestScriptloader.
	self markPackagesBeforeNewCodeIsLoaded.
	self checkImageIsUptodate ifFalse: [self loadLatestUpdateList].
	self checkImageIsUptodate ifFalse: [
		^ self inform: 'Your update.staged file and your image are not in sync! Do not stress! Normally it should be easy to fix.
		 
		If the integration failed previously: 
		- 1. Load the previous version of the Scriptloader, modify it to get the package dirty and republish it. It will get a newer number and the setting up of the integration will load it instead of a version of ScriptLoader containing an integration that did not finish.
		-2 just throw away your current directory. Do not worry about the fact that the changeset has been added to git. When you will integrate, the process will create a file with the same number and erase the committed one. 
		
		Note that if you have to rollback the created image you have to access the file repository to remove the latest image.
		The file server is available at
		 files.pharo.org
			Server:		193.51.235.3
			Address:	193.51.235.3#53
		It may be possible that you do not access and need to get your ssh files there first. 
		' ].
	self setUpdateAndScriptVersionNumbers.
	self saveAsNewImageWithCurrentReleaseName.
	self inform: 'The new version number is ' , self currentUpdateVersionNumber asString , '. Ready to apply changes now.', String cr, 'You are now running in image ', (Smalltalk imageFile basename).