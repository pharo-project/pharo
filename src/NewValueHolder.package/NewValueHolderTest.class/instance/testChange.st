tests
testChange
	| valueHolder fired |
	valueHolder := 'word' asValueHolder.
	self assert: valueHolder value equals: 'word'.
	fired := false.
	valueHolder whenChangedDo: [ fired := true ].
	valueHolder value: 'new'.
	self assert: fired.
	self assert: valueHolder value equals: 'new'