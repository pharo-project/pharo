cleaning
cleanResources
	"Clear all resources. This step is mandatory and happens immeditaly after the process has 
	started. And this must be called even if there is an error, since we must free pointers,
	close streams, etc etc. "
	self destroyPosixSpawnFileActionsT.
	self cleanAndFixCopiedStreams.
	self freePointers.