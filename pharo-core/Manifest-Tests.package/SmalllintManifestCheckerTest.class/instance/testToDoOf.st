tests
testToDoOf

	| rule |
	rule := RBCompositeLintRule allGoodRules.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.

	self assert: (( checker toDoOf: RBOnlyReadOrWrittenTemporaryRule new) anySatisfy: [:each| each = (MFClassB>>#method3)]).
	self deny: (( checker toDoOf: RBOnlyReadOrWrittenTemporaryRule new) anySatisfy: [:each| each = (MFClassB>>#method2)]).