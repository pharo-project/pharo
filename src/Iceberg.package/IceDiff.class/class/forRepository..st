instance creation
forRepository: aRepository 
	^ self new 
		repository: aRepository;
		theirVersion: (IceCurrentHead new repository: aRepository)
		yourself.