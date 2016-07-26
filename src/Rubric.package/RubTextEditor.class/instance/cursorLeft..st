nonediting/nontyping keys
cursorLeft: aKeyboardEvent 
	"Private - Move cursor left one character if nothing selected, otherwise 
	move cursor to beginning of selection. If the shift key is down, start 
	selecting or extending current selection. Don't allow cursor past 
	beginning of text"

	self closeTypeIn.
	self
		moveCursor:[:position | position - 1 max: 1]
		forward: false
		specialBlock:[:position | self previousWord: position]
		event: aKeyboardEvent.
	^ true