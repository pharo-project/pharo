editing keys
makeCapitalized: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-z."

	| prev |
	self closeTypeIn.
	prev := $-.  "not a letter"
	self replaceSelectionWith: (Text fromString:
		(self selection string collect:
			[:c | prev := prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]])).
	^ true