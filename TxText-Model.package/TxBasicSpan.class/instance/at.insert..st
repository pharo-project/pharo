operation
at: txPosition insert: aString
	"
	Insert a possibly multi-line string at given position of receiver.
	Answer the position past the last character of inserted string.
	The inserted portion of text must have same attributes as insertion point.
	"

	| position first insertAttributes |
	position := txPosition.
	first := true.
	insertAttributes := self styleAtPosition: txPosition.

	self string: aString linesDo: [:chars |
		
		first 
			ifTrue: [ first := false. ]
			ifFalse: [ 
				| separator |
				separator := text newLineSeparator replaceAttributes: insertAttributes.
				position := position span insertLineSeparator: separator at: position ].

		chars ifNotEmpty: [
			position := position span insertCharacters: chars at: position attributes: insertAttributes
			]  
	].
	^ position