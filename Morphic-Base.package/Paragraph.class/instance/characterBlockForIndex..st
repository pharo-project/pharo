selection
characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line |
	line := lines at: (self lineIndexForCharacter: index).
	^ (CharacterBlockScanner new text: text textStyle: textStyle)
		characterBlockAtPoint: nil index: ((index max: line first) min: text size+1)
		in: line