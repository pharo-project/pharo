tests
testAsClassRuleTransformation
	| asClassMethod critique |
	
	asClassMethod := testClass >> (testClass compile:
		'aMethodWhichSendsAsClass 
			#', self class name, ' asClass').
	
	critique := asClassMethod critiques detect: [ :crit |
		crit rule class = AsClassRule ].
	
	self assert: critique change class equals: RBAddMethodChange.
	self assert: (critique change source includesSubstring: 'self class environment at:')
	