startup - shutdown
runShutdown: isImageQuitting
	self
		runList: manager shutdownList
		do: [ :each | each shutdown: isImageQuitting ]