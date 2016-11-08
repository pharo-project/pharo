tests
testToDoOf

	| rule |
	rule := ReRuleManager visibleRuleClasses collect: #new.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
		
	self assert: (( checker toDoOf: GRTemporaryNotReadNorWrittenRule new) anySatisfy: [:each|
		each sourceAnchor entity = (MFClassB>>#method3)]).
	self deny: (( checker toDoOf: GRTemporaryNotReadNorWrittenRule new) anySatisfy: [:each|
		each  sourceAnchor entity = (MFClassB>>#method2)]).