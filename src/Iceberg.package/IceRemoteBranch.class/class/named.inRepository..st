instance creation
named: branchName inRepository: repo
	^ self new 
		name: ((branchName withoutPrefix: 'refs/') withoutPrefix: 'remotes/'); 
		repository: repo; 
		yourself