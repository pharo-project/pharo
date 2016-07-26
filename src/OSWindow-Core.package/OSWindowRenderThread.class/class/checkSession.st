session management
checkSession
	RenderSession ~~ Smalltalk session ifTrue: [ 
		self createRenderThread.
		RenderSession := Smalltalk session.
	]
