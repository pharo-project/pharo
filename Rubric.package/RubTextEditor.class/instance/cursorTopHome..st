typing/selecting keys
cursorTopHome: aKeyboardEvent
	"Put cursor at beginning of text -- invoked from cmd-H shortcut, useful for keyboards that have no home key."

	self selectAt: 1.
	^ true