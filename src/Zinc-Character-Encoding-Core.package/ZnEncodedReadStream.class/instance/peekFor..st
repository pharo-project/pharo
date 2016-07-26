accessing
peekFor: object
	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]