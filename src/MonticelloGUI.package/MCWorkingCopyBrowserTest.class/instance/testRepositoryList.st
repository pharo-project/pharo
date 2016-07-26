tests
testRepositoryList
	self assert: b repositoryList notNil.
	self assert: b repositoryList notEmpty. 
	self assert: b repositoryList size equals: b repositories size.
	self assert: (b repositoryList allSatisfy: [ :r | r isString ]).