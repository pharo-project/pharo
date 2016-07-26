testing
isEmpty
	iterations isNil | elapsedTime isNil 
		ifTrue: [ ^ true ].
	iterations isZero | elapsedTime isZero 
		ifTrue: [ ^ true ].
	^ false