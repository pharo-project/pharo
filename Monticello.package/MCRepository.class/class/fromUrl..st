instance creation
fromUrl: aUrlOrString 
	| url |
	url := aUrlOrString asUrl.

	^ self allSubclasses 
		detect: [ :each | 
			(each class includesSelector: #urlSchemes)
			and: [ each urlSchemes includes: url scheme ] ]
		ifFound: [ :repositoryClass | repositoryClass basicFromUrl: url ]
		ifNone: [ MCRepositoryError signal: 'Cannot instantiate a repository from the url: ', url asString ]