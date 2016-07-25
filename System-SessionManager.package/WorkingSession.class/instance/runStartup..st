startup - shutdown
runStartup: isImageStarting
	self
		runList: manager startupList
		do: [ :each | each startup: isImageStarting ].
	self executeDeferredStartupActions: isImageStarting
	