editing keys
indent: aKeyboardEvent
	"Add a tab at the front of every line occupied by the selection."

	^ self inOutdent: aKeyboardEvent delta: 1