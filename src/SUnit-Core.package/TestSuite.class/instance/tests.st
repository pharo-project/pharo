accessing
tests
	tests isNil ifTrue: [tests := OrderedCollection new].
	^tests