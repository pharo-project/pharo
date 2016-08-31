utilities
isErrorTest: aMethodOrMethodDefinition
	^ aMethodOrMethodDefinition methodClass isTestCase
		and: [ aMethodOrMethodDefinition methodClass
				methodRaisedError: aMethodOrMethodDefinition selector ]