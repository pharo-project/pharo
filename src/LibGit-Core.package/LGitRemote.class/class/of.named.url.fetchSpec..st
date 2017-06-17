instance creation
of: aRepository named: nameString url: urlString fetchSpec: aRefSpec
	^ (self of: aRepository)
		remoteName: nameString;
		url: urlString;
		fetchSpec: aRefSpec;
		yourself