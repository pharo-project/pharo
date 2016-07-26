*Morphic-Widgets-Taskbar
taskbarButtonClicked
	"The taskbar button for the receiver has been clicked.
	If minimised then restore.
	If active then minimize.
	Otherwise make active."

	self isMinimized
		ifTrue: [self restore]
		ifFalse: [self isActive
					ifTrue: [self minimize]
					ifFalse: [self activate]]