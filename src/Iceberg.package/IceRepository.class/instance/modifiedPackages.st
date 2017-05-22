accessing
modifiedPackages
	^ self loadedPackages 
		select: [ :package | package workingCopy needsSaving ]