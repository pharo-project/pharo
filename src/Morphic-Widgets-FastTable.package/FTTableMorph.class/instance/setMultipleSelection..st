accessing
setMultipleSelection: aBoolean

	aBoolean
		ifTrue: [ self beMultipleSelection ]
		ifFalse: [ self beSingleSelection ]