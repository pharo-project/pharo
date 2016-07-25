results
linkTo: stackOrNil

	stackOrNil ifNil: [^  self class newOn: self].
	^ self position = stackOrNil start
		ifTrue: [stackOrNil]
		ifFalse: [self error: 'stack out of sync in bytecode generator']