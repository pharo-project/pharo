styling
emphasisAsArray
	^ self emphasis = 'bold italic'
		ifTrue: [#(#bold #italic )]
		ifFalse: [self emphasis]