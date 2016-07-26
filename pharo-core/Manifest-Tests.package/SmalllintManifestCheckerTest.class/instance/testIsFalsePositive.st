tests
testIsFalsePositive 

	| rule |
	rule  := RBCompositeLintRule allGoodRules.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.  
	self assert: (checker isFalsePositive:  (MFClassB>>#method3) forRuleId: (RBCodeCruftLeftInMethodsRule uniqueIdentifierName) versionId:  1).
	self deny: (checker isFalsePositive:  (MFClassA>>#method) forRuleId: (RBCodeCruftLeftInMethodsRule uniqueIdentifierName) versionId:  1).

