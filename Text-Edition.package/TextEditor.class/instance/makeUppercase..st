editing keys
makeUppercase: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-Y."

	self closeTypeIn.
	self replaceSelectionWith: (Text fromString: (self selection string asUppercase)).
	^ true