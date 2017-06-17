instance creation
url: aRepositoryUrl
	self allSubclasses  
		detect: [ :subclass | subclass canHandleUrl: aRepositoryUrl ]
		ifFound: [ :subclass | ^ subclass new url: aRepositoryUrl ]
		ifNone: [ IceWrongUrl signal: 'Remote repository scheme not supported: ', aRepositoryUrl ]