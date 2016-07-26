tests
testIsToDo

	| rule |
	rule  := RBCompositeLintRule allGoodRules.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
	
	self assert: (checker isToDo:  (MFClassB>>#method3) forRuleId: (RBOnlyReadOrWrittenTemporaryRule uniqueIdentifierName) versionId:  1).
	self deny: (checker isToDo:  (MFClassB>>#method2) forRuleId: (RBOnlyReadOrWrittenTemporaryRule uniqueIdentifierName) versionId:  1).

