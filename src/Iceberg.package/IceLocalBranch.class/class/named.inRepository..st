instance creation
named: branchName inRepository: repo
	^ self new 
		name: (branchName withoutPrefix: 'refs/heads/'); 
		repository: repo; 
		yourself