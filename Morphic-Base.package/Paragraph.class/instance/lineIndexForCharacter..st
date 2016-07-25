private
lineIndexForCharacter: index
	"Answer the index of the line in which to select the character at index."
	^ (self fastFindFirstLineSuchThat: [:line | line first > index]) - 1 max: 1