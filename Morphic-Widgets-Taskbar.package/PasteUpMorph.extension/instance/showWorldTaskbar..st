*Morphic-Widgets-Taskbar
showWorldTaskbar: aBoolean
	"Change the receiver to show the taskbar."
	
	aBoolean
		ifTrue: [self createTaskbarIfNecessary]
		ifFalse: [self removeTaskbar]