utilities
correspondingTestMethodOf: aMethodOrMethodDefinition
	| selectorString testCase |
	aMethodOrMethodDefinition isTestMethod
		ifTrue: [ ^ aMethodOrMethodDefinition ].
	selectorString := 'test'
		, aMethodOrMethodDefinition selector transformSelectorForComparison.
	(testCase := aMethodOrMethodDefinition methodClass correspondingTestCase)
		ifNil: [ ^ nil ].
	^ testCase methods
		detect: [ :each | each selector transformSelectorForComparison = selectorString ]
		ifNone: [ nil ]