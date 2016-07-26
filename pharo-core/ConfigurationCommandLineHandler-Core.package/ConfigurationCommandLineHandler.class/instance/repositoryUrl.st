accessing
repositoryUrl
	^ repositoryURL ifNil: [ 
		self hasRepositoryUrl 
			ifFalse: [ ^ self defaultRepositoryUrl ].
		repositoryURL := self loadRepositoryUrl ]