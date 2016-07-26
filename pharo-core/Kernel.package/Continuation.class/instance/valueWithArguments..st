evaluating
valueWithArguments: anArray
	anArray size = 1
		ifFalse: [ ^ self error: 'continuations can only be resumed with one argument' ].
	self value: anArray first