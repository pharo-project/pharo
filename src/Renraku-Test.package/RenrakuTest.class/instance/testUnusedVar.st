tests
testUnusedVar

	testClass superclass
		subclass: testClass name
		instanceVariableNames: 'unused'
		classVariableNames: ''
		category: testClass category.

	self assert: (testClass critiques anySatisfy: [ :critic |
		critic rule class = RBOnlyReadOrWrittenVariableRule ]).
	
	testClass superclass
		subclass: testClass name
		instanceVariableNames: ''
		classVariableNames: ''
		category: testClass category.
		
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = RBOnlyReadOrWrittenVariableRule ]).

