private
runClassTests: aCollection notifying: aBoolean

	(aCollection allSatisfy: [:each | each isTestCase ]) ifFalse: [ ^ self ].
	aCollection do: [:class | self runTestsOfClass: class notifying: aBoolean ]