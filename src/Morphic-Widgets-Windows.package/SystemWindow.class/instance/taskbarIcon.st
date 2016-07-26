icons
taskbarIcon
	"Answer the icon for the receiver in a task bar."

	self model ifNotNil: [self model taskbarIcon ifNotNil: [:ico | ^ico]].
	^super taskbarIcon