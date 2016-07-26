testing
expectedFailures
	| pragmas |
	^ expectedFails ifNil: [
		
		pragmas := Pragma allNamed: #expectedFailure from: self class to: TestCase.
		expectedFails := pragmas collect: [:each | each method selector  ].
	]	
