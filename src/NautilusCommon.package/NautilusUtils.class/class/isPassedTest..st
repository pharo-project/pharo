utilities
isPassedTest: aMethodOrMethodDefinition
	^ aMethodOrMethodDefinition methodClass isTestCase
		and: [ aMethodOrMethodDefinition methodClass
				methodPassed: aMethodOrMethodDefinition selector ]