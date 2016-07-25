testing
runTest: aSelector

	| actualResult expectedResult |
	[ self 
		logTest: aSelector;
		clearLog;
		perform: aSelector ]
			on: MyTestError do: 
				[ :ex | self log: 'Unhandled Exception'.
					ex return: nil ].

	actualResult	:= self log.
	expectedResult := self perform: (aSelector, #Results) asSymbol.

	actualResult = expectedResult
		ifTrue: [self logTestResult: 'succeeded']
		ifFalse: [self logTestResult: 'failed' ].
