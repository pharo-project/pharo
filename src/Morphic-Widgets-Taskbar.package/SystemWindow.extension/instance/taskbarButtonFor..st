*Morphic-Widgets-Taskbar
taskbarButtonFor: aTaskbar
	"Answer a new task bar button for the receiver.
	Answer nil if not required."

	^aTaskbar theme
		newTaskbarButtonIn: aTaskbar
		for: self