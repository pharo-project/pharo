Class {
	#name : #ReSmalllintCheckerTest,
	#superclass : #ReCodeBasedTestCase,
	#category : #'Renraku-Tests'
}

{ #category : #tests }
ReSmalllintCheckerTest >> testRun [
	| checker |
	
	checker := ReSmalllintChecker new
		rule: { ScreamerRule new };
		environment: testPackage asEnvironment;
		yourself.
		
	checker run.
	
	self assert: (checker criticsOf: ScreamerRule new) notEmpty.
]
