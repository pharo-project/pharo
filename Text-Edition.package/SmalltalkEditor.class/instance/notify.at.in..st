new selection
notify: aString at: anInteger in: aStream 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."

	| pos |
	pos := self selectionInterval notEmpty
		ifTrue: [self startIndex + anInteger - 1 ]
		ifFalse: [anInteger].
	self insertAndSelect: aString at: (pos max: 1)