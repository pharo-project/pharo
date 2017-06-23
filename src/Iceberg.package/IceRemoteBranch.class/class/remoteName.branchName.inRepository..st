instance creation
remoteName: remoteName branchName: branchName inRepository: aRepository 
	^ self new 
		name: remoteName, '/', branchName; 
		repository: aRepository; 
		yourself