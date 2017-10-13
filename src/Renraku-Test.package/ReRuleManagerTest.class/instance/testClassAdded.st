tests
testClassAdded

	| rule rulesCount |
	
	ReRuleManager reset.
	
	rulesCount := ReRuleManager default nodeRules size.
	
	rule := ReNodeMatchRule 
		subclass: #ReRuleManagerTestingRule
		instanceVariableNames: ''
		classVariableNames: ''
		category: testPackage name.
		
	rule compile: 'name ^ ''test'''.

	self
		assert: ReRuleManager default nodeRules size - rulesCount
		equals: 1.

	self assert:
		(ReRuleManager default allRules anySatisfy: [ :r |
			r class == rule ]).
		
	rule removeFromSystem.