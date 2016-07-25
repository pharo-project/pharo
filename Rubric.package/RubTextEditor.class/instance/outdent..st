editing keys
outdent: aKeyboardEvent
	"Remove a tab from the front of every line occupied by the selection."

	^ self inOutdent: aKeyboardEvent delta: -1