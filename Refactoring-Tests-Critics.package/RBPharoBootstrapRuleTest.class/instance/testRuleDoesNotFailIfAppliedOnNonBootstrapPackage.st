tests
testRuleDoesNotFailIfAppliedOnNonBootstrapPackage
	| rule |
	rule := RBPharoBootstrapRule new.
	rule class 
		classVarNamed: 'DependencyChecker'
		put: RBPharoBootstrapAnalyzerStubWithNoNewDependency.

	rule checkPackage: (RPackage named: #P1).

	self assert: rule result packages isEmpty