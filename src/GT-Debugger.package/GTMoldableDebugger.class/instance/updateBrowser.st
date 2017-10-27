updating
updateBrowser
	| browserPane session |
	
	session := self session.
	session interruptedContext ifNil: [ 
		self close.
		UIManager default inform: 'End of debugged execution'.
		 ] ifNotNil: [  
	browserPane := self browser pane.
	(browserPane port: #entity) silentValue: nil.
	(browserPane port: #entity) value: session. ]