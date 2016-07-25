*Morphic-Widgets-Taskbar
taskbarLabel
	"Answer the label to use for a taskbar button for the receiver."

	self model ifNotNil: [self model taskbarLabel ifNotNil: [:str | ^str]].
	^self labelString