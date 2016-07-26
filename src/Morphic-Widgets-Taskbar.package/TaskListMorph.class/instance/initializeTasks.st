initialization
initializeTasks
	"Set up the current tasks."
	
	self tasks: ((World submorphs
		collect: [:m | m taskbarTask])
		select: [:m | m notNil]) asOrderedCollection