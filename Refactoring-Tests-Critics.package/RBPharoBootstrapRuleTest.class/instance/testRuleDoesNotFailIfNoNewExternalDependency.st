tests
testRuleDoesNotFailIfNoNewExternalDependency
	| rule |
	rule := RBPharoBootstrapRule new.
	rule class 
		classVarNamed: 'DependencyChecker'
		put: RBPharoBootstrapAnalyzerStubWithNoNewDependency.

	rule checkPackage: (RPackage named: #Kernel).

	self assert: rule result packages isEmpty