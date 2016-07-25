childs management
activeChildren
	"Answer child processes which are currently believed to be running."

	^ accessProtect critical: [ childProcessList select: [ :process | process isRunning ] ].
