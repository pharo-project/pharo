asserting
assert: aBooleanOrBlock description: aStringOrBlock
	aBooleanOrBlock value ifFalse: [
		| message |
		message := aStringOrBlock value.
		self classForTestResult failure signal: message]
			