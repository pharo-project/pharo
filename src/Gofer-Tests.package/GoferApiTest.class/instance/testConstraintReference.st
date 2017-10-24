tests - references
testConstraintReference
	gofer 
		repository: self monticelloRepository; 
		package: 'GoferBar' constraint: [ :ref | ref branch = 'branch' ].
	self assert: (gofer resolved size = 1).
	self assert: (gofer resolved first isKindOf: GoferResolvedReference).
	self assert: (gofer resolved first packageName = 'GoferBar').
	self assert: (gofer resolved first author = 'lr').
	self assert: (gofer resolved first branch = 'branch').
	self assert: (gofer resolved first versionNumber = 2).
	self assert: (gofer resolved first repository = self monticelloRepository)