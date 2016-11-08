tests
testIsToDo

	| rule |
	rule  := ReRuleManager visibleRuleClasses collect: #new.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
	
	self assert: (checker isToDo:  (MFClassB>>#method3) forRuleId: (GRTemporaryNotReadNorWrittenRule uniqueIdentifierName) versionId:  1).
	self deny: (checker isToDo:  (MFClassB>>#method2) forRuleId: (GRTemporaryNotReadNorWrittenRule uniqueIdentifierName) versionId:  1).

