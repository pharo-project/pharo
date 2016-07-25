private
waitingCacheFolder
	^ self packageToBeTestedFolderName asFileReference 
		ensureCreateDirectory;
		yourself

	