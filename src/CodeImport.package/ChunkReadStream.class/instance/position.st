decorated
position
	
	^ decoratedStream position - (nextChar ifNil: [0] ifNotNil: [1])