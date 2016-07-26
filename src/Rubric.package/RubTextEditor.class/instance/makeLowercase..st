editing keys
makeLowercase: aKeyboardEvent
	"Force the current selection to lowercase.  Triggered by Cmd-X."

	self closeTypeIn.
	self replaceSelectionWith: (Text fromString: (self selection string asLowercase)).
	^ true