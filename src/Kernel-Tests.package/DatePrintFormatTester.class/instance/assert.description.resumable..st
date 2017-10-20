asserting
assert: aBooleanOrBlock description: aStringOrBlock resumable: resumableBoolean 
	| exception |
	aBooleanOrBlock value
		ifFalse: 
			[|aString|
			aString := aStringOrBlock value.
			exception := resumableBoolean
						ifTrue: [self classForTestResult resumableFailure]
						ifFalse: [self classForTestResult failure].
			exception signal: aString]
			