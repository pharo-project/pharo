*Morphic-Widgets-Taskbar
removeTaskbar
	"Remove the receiver's taskbars."
	
	self taskbars do: [:each | each removeFromWorld]