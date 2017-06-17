instance creation
repository: aRepository credentials: aCredentials headRemote: aRemote
	^ self basicNew 
		initializeRepository: aRepository 
			credentials: aCredentials
			headRemote: aRemote;
		yourself