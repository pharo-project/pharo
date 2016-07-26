events-processing
eventLoopProcessWithPlugin
	| event session semaIndex |
	event := SDL_Event new.
	session := Smalltalk session.
	
	semaIndex := (Smalltalk registerExternalObject: inputSemaphore).
	self primSetupInputSemaphore: semaIndex.
	[
		[ inputSemaphore wait; consumeAllSignals. session  == Smalltalk session] whileTrue: [
			[ (SDL2 pollEvent: event) > 0 ] whileTrue: [
				self processEvent: event
			].
		]
	] ensure:  [ Smalltalk unregisterExternalObject: inputSemaphore ]