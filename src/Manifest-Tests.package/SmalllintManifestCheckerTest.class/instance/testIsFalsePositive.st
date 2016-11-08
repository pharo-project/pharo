tests
testIsFalsePositive 

	| rule |
	rule  := ReRuleManager visibleRuleClasses collect: #new.
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.  
		
	self assert: (checker isFalsePositive:  (MFClassB>>#method3) forRuleId: (RBCodeCruftLeftInMethodsRule uniqueIdentifierName) versionId:  1).
	self deny: (checker isFalsePositive:  (MFClassA>>#method) forRuleId: (RBCodeCruftLeftInMethodsRule uniqueIdentifierName) versionId:  1).

