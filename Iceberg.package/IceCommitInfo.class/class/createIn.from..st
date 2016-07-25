instance creation
createIn: anIceRepository from: aGitLogLine
	^ self new 
		initializeFrom: aGitLogLine;
		addToRepository: anIceRepository;
		yourself.
