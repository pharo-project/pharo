nonediting - nontyping keys
cursorHome: aKeyboardEvent 

	"Private - Move cursor from position in current line to beginning of
	current line. If control key is pressed put cursor at beginning of text"

	self closeTypeIn.
	self
		moveCursor: [ :position |
				(string
					lastIndexOf: Character cr
					startingAt: position - 1
					ifAbsent:[0]) + 1]
		forward: false
		specialBlock: [:dummy | 1]
		event: aKeyboardEvent.
	^true