as yet unclassified
testClassRemoved

	| rule |
	
	rule := RBLintRule
		subclass: #ReRuleManagerTestingRule
		instanceVariableNames: ''
		classVariableNames: ''
		category: testPackage name.
	rule class
		compile: 'checksMethod ^ true';
		compile: 'isVisible ^ true'.
		
	rule compile: 'name ^ ''test'''.

	ReRuleManager uniqueInstance resetRules.

	self assert:
		(ReRuleManager uniqueInstance allRules anySatisfy: [ :r |
			r class == rule ]).
		
	rule removeFromSystem.
		
	self assert:
		(ReRuleManager uniqueInstance allRules noneSatisfy: [ :r |
			r class == rule ]).