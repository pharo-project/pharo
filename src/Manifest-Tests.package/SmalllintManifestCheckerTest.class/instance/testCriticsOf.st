tests
testCriticsOf

	| rule critiques |
	rule := ReRuleManager visibleRuleClasses collect: #new.
	checker 
		rule: rule;
		environment: self package asEnvironment;
		run. 
	
	critiques := checker criticsOf: GRTemporaryNeitherReadNorWrittenRule new.
	
	self assert: critiques size  = 3.
	self assert: (critiques anySatisfy: [ :each |
		each sourceAnchor entity = (MFClassB>>#method3)]).
	self assert: (critiques anySatisfy: [ :each |
		each sourceAnchor entity = (MFClassA>>#method)]).
