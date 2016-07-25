instance creation
version: aMCVersion repository: aMCRepository

	^ self basicNew
		initializeWithVersion: aMCVersion
		repository: aMCRepository;
		yourself