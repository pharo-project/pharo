tests
testClassRemoved

	| rule |
	
	rule := ReAbstractRule 
		subclass: #ReRuleManagerTestingRule
		instanceVariableNames: ''
		classVariableNames: ''
		category: testPackage name.
	rule class
		compile: 'checksMethod ^ true'.
		
	rule compile: 'name ^ ''test'''.

	ReRuleManager reset.

	self assert:
		(ReRuleManager default allRules anySatisfy: [ :r |
			r class == rule ]).
		
	rule removeFromSystem.
		
	self assert:
		(ReRuleManager default allRules noneSatisfy: [ :r |
			r class == rule ]).