tests
testUnclassifiedMethod

	self assert: (testMethod critiques anySatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).
	
	testMethod protocol: 'some protocol'.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).