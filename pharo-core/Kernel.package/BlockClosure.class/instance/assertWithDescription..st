exceptions
assertWithDescription: aStringOrABlock
	| value |
	self value
		ifTrue: [ ^ self ].
	value := aStringOrABlock value.
	AssertionFailure signal: value