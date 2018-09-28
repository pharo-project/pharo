configuration
fromSemanticVersion: aString 
	
	| version versionAndSuffix partsWithoutSuffix |
	version := aString.
	(version beginsWith: 'v')
		ifTrue: [ version := version allButFirst ].
	
	versionAndSuffix := version substrings: '-'.
	partsWithoutSuffix := versionAndSuffix first substrings: '.'.
	
	partsWithoutSuffix size < 1
		ifTrue: [ self invalidMajorVersion: version ].
	self major: partsWithoutSuffix first.
	
	partsWithoutSuffix size > 1
		ifTrue: [ self minor: partsWithoutSuffix second ].
	
	partsWithoutSuffix size > 2
		ifTrue: [ self patch: partsWithoutSuffix third ].
	
	versionAndSuffix size > 1
		ifTrue: [ self suffix: versionAndSuffix second ].
	
	^ self