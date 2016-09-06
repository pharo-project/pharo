utilities
correspondingTestMethodOf: aMethodOrMethodDefinition
	| selectorString testCase |
	aMethodOrMethodDefinition isTestMethod
		ifTrue: [ ^ nil ].
	selectorString := 'test'
		, aMethodOrMethodDefinition selector normalizedSelectorForComparison.
	(testCase := aMethodOrMethodDefinition methodClass correspondingTestCase)
		ifNil: [ ^ nil ].
	^ testCase methods
		detect: [ :each | each selector normalizedSelectorForComparison = selectorString ]
		ifNone: [ nil ]