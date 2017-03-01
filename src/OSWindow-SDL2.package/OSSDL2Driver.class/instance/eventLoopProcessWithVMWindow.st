events-processing
eventLoopProcessWithVMWindow
	| event session semaIndex eventBuffer |
	event := SDL_Event new.
	eventBuffer := event getHandle. 
	session := Smalltalk session.
	
	semaIndex := (Smalltalk registerExternalObject: inputSemaphore).
	self primitiveSetVMSDL2Input: semaIndex.
	[
		[ inputSemaphore wait; consumeAllSignals. session  == Smalltalk session] whileTrue: [
			[ self primitivePollVMSDL2Event: eventBuffer size: eventBuffer size ] whileTrue: [
				self processEvent: event
			].
		]
	] ensure:  [ Smalltalk unregisterExternalObject: inputSemaphore ]