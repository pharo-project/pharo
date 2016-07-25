*Morphic-Widgets-Taskbar
taskbars
	"Answer the receiver's taskbars."
	
	^self submorphs select: [:each |
		each isTaskbar]