window animation
createdWindow: window
	self checkSession.
	RegisteredWindowsMutex critical: [
		RegisteredWindows add: window
	]
