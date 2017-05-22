instance creation
createFrom: aGitLogLine
	^ self new 
		initializeFrom: aGitLogLine;
		yourself.
