accessing-convenience
value
	^ self transformation isNil ifTrue: [self port value] ifFalse: [self transformation value: self port value]