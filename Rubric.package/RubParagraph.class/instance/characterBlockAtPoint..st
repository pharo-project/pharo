querying
characterBlockAtPoint: aPoint 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line := self lines at: (self lineIndexForPoint: aPoint).
	^ (RubCharacterBlockScanner new text: self text textStyle: self textStyle)
		characterBlockAtPoint: aPoint index: nil
		in: line