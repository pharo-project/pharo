accessing
fontOf: aCharacter ifAbsent: aBlock
	"Answer the actual font to use for aCharacter"
	| encoding font |
	encoding := aCharacter leadingChar + 1.
	encoding <= fontArray size 
		ifTrue:[font := fontArray at: encoding].
	font ifNil:[^aBlock value].
	^font
