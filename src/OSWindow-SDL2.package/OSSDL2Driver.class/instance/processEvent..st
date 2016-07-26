events-processing
processEvent: sdlEvent
	| event |
	[  
		event := self convertEvent: sdlEvent.
		event ifNotNil: [ eventQueue nextPut: event ].
	] on: Error do: [ :err | 
		"It is critical, that event handling keep running despite errors.
		Normally, any errors in event handling requires immediate attention and fixing"
		err freeze.
		[ err debug ] fork.
	 ]