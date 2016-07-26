initializing
reset
	"Remove the taskbar and add a new one." 
	
	(World submorphs select: [:m | m isKindOf: self])
		do: [:tb | tb delete].
	World createTaskbarIfNecessary