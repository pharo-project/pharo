testing
testOriginWithRequiredMethod
	| tr1 c1 |
	tr1 := self createTraitNamed: #TTT1 uses: {}.
	
	tr1 compile: 'foo ^ self explicitRequirement'.
	self assert: (tr1 >> #foo) origin == tr1.
	
	c1 := self createClassNamed: #CTT1 superclass: Object uses: tr1.
	self assert: (c1 >> #foo) origin == tr1.