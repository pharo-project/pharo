startup - shutdown
stop: isImageQuitting

	Object flushDependents.
	self runShutdown: isImageQuitting.