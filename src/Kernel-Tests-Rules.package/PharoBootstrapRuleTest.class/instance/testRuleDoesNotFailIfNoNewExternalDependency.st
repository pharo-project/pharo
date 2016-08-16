tests
testRuleDoesNotFailIfNoNewExternalDependency
	| rule critiques |
	rule := PharoBootstrapRule new.
	rule class 
		classVarNamed: 'DependencyChecker'
		put: PharoBootstrapAnalyzerStubWithNoNewDependency.

	critiques := rule check: (RPackage named: #Kernel).

	self assert: critiques isEmpty