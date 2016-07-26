class initialization
initialize
	"Callback initialize"
	self ensureNotifiedAtStartup.
	[self startUp: false] "Monticello... grrr...."
		on: Error
		do: [:ex| ].
	(Smalltalk specialObjectsArray at: 54) ~~ #invokeCallbackContext: ifTrue:
		[UIManager default inform: 'this version of Callbacks requires the callback selector to be\#invokeCallbackContext:.  recreate the specialObjects Array.' withCRs]