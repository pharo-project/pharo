asserting
assert: aBoolean
	aBoolean ifFalse:
		[self logFailure: 'Assertion failed'.
		self defaultTestFailure signal: 'Assertion failed'].