tests
testRepositories
	self assert: b repositories notNil.
	self assert: b repositories notEmpty.
	self assert: b repositoryGroup notNil.
	self assert: b repositoryGroup repositories size equals: b repositories size. 