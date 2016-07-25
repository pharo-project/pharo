querying
lineIndexOfCharacterIndex: characterIndex 
	"Answer the line index for a given characterIndex."
	^ (self fastFindFirstLineSuchThat: [:line | line first > characterIndex]) - 1 max: 1