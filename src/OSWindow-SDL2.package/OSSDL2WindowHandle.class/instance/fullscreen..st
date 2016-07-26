accessing
fullscreen: aBoolean
	aBoolean ifTrue: [
		handle fullscreen: SDL_WINDOW_FULLSCREEN_DESKTOP
	] ifFalse: [ 
		handle fullscreen: 0
	]