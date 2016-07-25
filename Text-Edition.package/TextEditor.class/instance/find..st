typing/selecting keys
find: aKeyboardEvent
	"Prompt the user for what to find, then find it, searching from the current selection onward."

	self closeTypeIn.
	self find.
	^ true