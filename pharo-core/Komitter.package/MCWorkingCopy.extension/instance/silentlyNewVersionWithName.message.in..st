*Komitter
silentlyNewVersionWithName: nameString message: messageString in: aRepository
	
	| info deps |
	info := ancestry infoWithName: nameString message: messageString.
	ancestry := MCWorkingAncestry new addAncestor: info.
	self modified: true.
	
	deps := self collectDependenciesWithMessage: messageString in: aRepository.
	
	(self repositoryGroup includes: aRepository) 
		ifFalse: [ self repositoryGroup addRepository: aRepository ].

	^ MCVersion
		silentlyPackage: package
		info: info
		snapshot: package snapshot
		dependencies: deps