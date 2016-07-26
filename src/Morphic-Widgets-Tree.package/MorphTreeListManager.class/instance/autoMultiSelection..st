accessing
autoMultiSelection: aBoolean
	autoMultiSelection := aBoolean.
	aBoolean ifTrue: [self multipleSelection: true]