tests
testValuesInBlock
	| valueHolder old new ann |
	valueHolder := 'old' asValueHolder.
	valueHolder
		whenChangedDo: [ :newValue :oldValue :a | 
			new := newValue.
			old := oldValue.
			ann := a ].
	valueHolder value: 'new'.
	self assert: old equals: 'old'.
	self assert: new equals: 'new'.
	self assert: (ann isMemberOf: ValueChanged)