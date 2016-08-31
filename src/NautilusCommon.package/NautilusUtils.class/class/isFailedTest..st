utilities
isFailedTest: aMethodOrMethodDefinition
	^ aMethodOrMethodDefinition methodClass isTestCase
		and: [ aMethodOrMethodDefinition methodClass
				methodFailed: aMethodOrMethodDefinition selector ]