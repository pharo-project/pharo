nonediting/nontyping keys
cursorEnd: aKeyboardEvent

	"Private - Move cursor end of current line."
	self closeTypeIn.
	self
		moveCursor:
			[ :position | 
				string
					indexOf: Character cr
					startingAt: position
					ifAbsent:[string size + 1]]
		forward: true
		specialBlock: [:dummy | string size + 1]
		event: aKeyboardEvent.
	^true