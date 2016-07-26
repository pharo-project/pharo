instance creation
forMCRepository: anIceMCGitRepository 
	^ (self origin: anIceMCGitRepository remoteUrl)
		repository: anIceMCGitRepository