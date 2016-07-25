repository handling
searchExistingRepository
	^ IceMCGitRepository allInstances 
		detect: [ :repo | repo remoteUrl = self origin ]
		ifFound: [ :repo | repo validate ]
		ifNone: [ nil ]