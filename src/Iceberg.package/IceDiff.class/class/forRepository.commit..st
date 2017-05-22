instance creation
forRepository: aRepository commit: anIceCommitInfo
	^ self new 
		repository: aRepository;
		theirVersion: anIceCommitInfo;
		yourself.