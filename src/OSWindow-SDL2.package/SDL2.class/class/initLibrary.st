common
initLibrary
	Session == Smalltalk session ifTrue: [ ^ 1 ].
	self init: SDL_INIT_NOPARACHUTE.
	Session := Smalltalk session.