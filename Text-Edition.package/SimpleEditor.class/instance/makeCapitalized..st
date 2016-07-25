editing keys
makeCapitalized: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	| prev |
	prev := $-.  "not a letter"
	self replaceSelectionWith:
		(self selection collect: [:c |
			prev := prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).
	^ true