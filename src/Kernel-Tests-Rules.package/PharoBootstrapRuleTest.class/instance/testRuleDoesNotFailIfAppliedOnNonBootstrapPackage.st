tests
testRuleDoesNotFailIfAppliedOnNonBootstrapPackage
	| rule critiques |
	rule := PharoBootstrapRule new.
	rule class 
		classVarNamed: 'DependencyChecker'
		put: PharoBootstrapAnalyzerStubWithNoNewDependency.

	critiques := rule check: (RPackage named: #P1).

	self assert: critiques isEmpty