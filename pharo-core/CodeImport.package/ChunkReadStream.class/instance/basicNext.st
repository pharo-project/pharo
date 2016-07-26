decorated
basicNext

	nextChar = self terminatorMark 
		ifTrue: [ decoratedStream next ].
	^ nextChar