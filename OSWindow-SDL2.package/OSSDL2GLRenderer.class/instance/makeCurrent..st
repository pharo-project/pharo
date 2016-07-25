misc
makeCurrent: aWindow
	| windowHandle |
	context ifNil: [ ^ false ].
	
	self checkThread.
	windowHandle := aWindow handle.
	windowHandle ifNil: [ ^ false ].
	(SDL2 glMakeCurrent: windowHandle context: context) == 0
		ifTrue:  [ ^ true ]
		ifFalse:  [ ^ false ]
	