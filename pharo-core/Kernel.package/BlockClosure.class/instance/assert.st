exceptions
assert

	self value ifFalse: [AssertionFailure signal: 'Assertion failed']
