tests
testToDoOf

	| rule |
	rule := ReRuleManager visibleRuleClasses collect: #new.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
		
	self assert: (( checker toDoOf: GRTemporaryNeitherReadNorWrittenRule new) anySatisfy: [:each|
		each sourceAnchor entity = (MFClassB>>#method3)]).
	self deny: (( checker toDoOf: GRTemporaryNeitherReadNorWrittenRule new) anySatisfy: [:each|
		each  sourceAnchor entity = (MFClassB>>#method2)]).