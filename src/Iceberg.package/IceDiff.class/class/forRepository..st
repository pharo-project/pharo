instance creation
forRepository: aRepository 
	^ self new 
		repository: aRepository;
		myVersion: IceCommitish image;
		theirVersion: aRepository loadedCode;
		yourself.