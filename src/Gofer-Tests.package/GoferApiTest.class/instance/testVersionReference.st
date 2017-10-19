tests - references
testVersionReference
	gofer 
		repository: self monticelloRepository; 
		version: 'GoferFoo-lr.2'.
	self assert: (gofer resolved size = 1).
	self assert: (gofer resolved first isKindOf: GoferResolvedReference).
	self assert: (gofer resolved first packageName = 'GoferFoo').
	self assert: (gofer resolved first author = 'lr').
	self assert: (gofer resolved first branch isEmpty).
	self assert: (gofer resolved first versionNumber = 2).
	self assert: (gofer resolved first repository = self monticelloRepository)