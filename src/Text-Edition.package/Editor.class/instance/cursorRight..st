nonediting/nontyping keys
cursorRight: aKeyboardEvent 
	"Private - Move cursor right one character if nothing selected, 
	otherwise move cursor to end of selection. If the shift key is down, 
	start selecting characters or extending already selected characters. 
	Don't allow cursor past end of text"

	self closeTypeIn.
	self
		moveCursor: [:position |
			self class skipOverMultipleSpaces 
				ifTrue: [ self nextNonBlank: position ] 
				ifFalse: [ position + 1 ]
			]
		forward: true
		specialBlock:[:position | self nextWord: position]
		event: aKeyboardEvent.
	^ true