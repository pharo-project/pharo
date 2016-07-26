asserting
assert: aBooleanOrBlock description: aStringOrBlock
	aBooleanOrBlock value ifFalse: [
		| aString |
		aString := aStringOrBlock value.
		self logFailure: aString.
		self classForTestResult failure signal: aString]
			