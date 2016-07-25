events-processing
eventLoopProcessWithoutPlugin
	| event session |
	event := SDL_Event new.
	session := Smalltalk session.
	
	[ session == Smalltalk session] 
	whileTrue: [
		[ (SDL2 pollEvent: event) > 0 ] 
		whileTrue: [ self processEvent: event ].
		
		(Delay forMilliseconds: 5) wait ]