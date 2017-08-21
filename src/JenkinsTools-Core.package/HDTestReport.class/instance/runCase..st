running
runCase: aTestCase
	| error stack  |
	self
		beginTestCase: aTestCase
		runBlock: [ [ [ aTestCase runCaseManaged ]
			on: Halt, Error, TestFailure
			do: [ :err |
				error := err.
				aTestCase shouldPass ifTrue: [
					self
						serializeError: error
						of: aTestCase ].
				stack := self
					stackTraceString: err
					of: aTestCase ]]
						on: TestSkip
						do: [ :err | "nothing to do..." ] ].
	
	self
		writeError: error
		andStack: stack
		for: aTestCase.
	self endTestCase