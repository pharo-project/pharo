instance creation
versionName: aVersionName repositoryDescription: aRepositoryDescription 
	^ self basicNew
		initializeWithVersionName: aVersionName
		repositoryDescription: aRepositoryDescription;
		yourself