window animation
destroyedWindow: window
	self checkSession.
	RegisteredWindowsMutex critical:  [ 
		RegisteredWindows remove: window ifAbsent: [ nil ]
	]