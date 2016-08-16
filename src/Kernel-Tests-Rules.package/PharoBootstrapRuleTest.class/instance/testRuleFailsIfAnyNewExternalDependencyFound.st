tests
testRuleFailsIfAnyNewExternalDependencyFound
	| rule packageUnderTest critiques |
	rule := PharoBootstrapRule new.
	rule class 
		classVarNamed: 'DependencyChecker' 
		put: PharoBootstrapAnalyzerStubWithNewDependency.
	packageUnderTest := RPackage named: #Kernel.

	critiques := rule check: packageUnderTest.

	self assert: critiques size equals: 1.

	self
		assert: critiques anyOne sourceAnchor entity
		equals: packageUnderTest