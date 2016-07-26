querying
characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line |
	line := self lines at: (self lineIndexOfCharacterIndex: index).
	^ (RubCharacterBlockScanner new text: self text textStyle: self textStyle)
		characterBlockAtPoint: nil index: ((index max: line first) min: self text size+1)
		in: line