querying
lineIndexOfCharacterIndex: characterIndex 
	"Answer the line index for a given characterIndex."
	"apparently the selector changed with NewParagraph"
	^ self composer lineIndexOfCharacterIndex: characterIndex
