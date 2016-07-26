operation
insertNewLine: aString 
	"Insert a newline then the String"
	| newline chars |
	newline := text newLineSeparator.
	aString isEmpty ifTrue: [ self insertNext: newline. ^ newline ].
	chars := text newCharacterSpan: aString.
	newline next: chars.
	self insertNext: newline.
	^ chars