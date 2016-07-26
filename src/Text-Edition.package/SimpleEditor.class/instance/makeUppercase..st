editing keys
makeUppercase: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-Y."

	self replaceSelectionWith: (self selection asUppercase).
	^ true