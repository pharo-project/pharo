operation
splitAt: offset
	| newSpan |
	characters ifEmpty: [ ^ self ].

	newSpan := text newCharacterSpan: (characters allButFirst: offset).
	characters := characters first: offset.
	"also inherit style"
	newSpan setStyle: self style.
	
	^ self insertNext: newSpan.