tests
testRuleFailsIfAnyNewExternalDependencyFound
	| rule packageUnderTest |
	rule := RBPharoBootstrapRule new.
	rule class 
		classVarNamed: 'DependencyChecker' 
		put: RBPharoBootstrapAnalyzerStubWithNewDependency.
	packageUnderTest := RPackage named: #Kernel.

	rule checkPackage: packageUnderTest.

	self 
		assert: rule result packages 
		equals: (Set with: packageUnderTest ) 