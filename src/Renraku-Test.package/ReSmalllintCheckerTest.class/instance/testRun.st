tests
testRun
	| checker |
	
	checker := ReSmalllintChecker new
		rule: { ScreamerRule new };
		environment: testPackage asEnvironment;
		yourself.
		
	checker run.
	
	self assert: (checker criticsOf: ScreamerRule new) notEmpty.