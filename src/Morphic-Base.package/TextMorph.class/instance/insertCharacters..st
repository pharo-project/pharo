scripting access
insertCharacters: aSource
	"Insert the characters from the given source at my current cursor position"

	| aLoc |
	aLoc := self cursor max: 1.
	paragraph replaceFrom: aLoc to: (aLoc - 1) with: aSource asText displaying: true.
	self updateFromParagraph  