tests
testBeta
	self setUpGreek.
	DeleteVisitor delete: (filesystem / 'alpha' / 'beta').
	self assert: (filesystem isDirectory: '/alpha').
	self assert: (filesystem isDirectory: '/alpha/epsilon').
	self deny: (filesystem exists: '/alpha/beta').
	