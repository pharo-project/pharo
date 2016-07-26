parsing
parseMethod: aString onError: aBlock 
	| parser |
	parser := self new.
	parser errorBlock: aBlock.
	parser initializeParserWith: aString.
	^ [ parser parseMethod: aString ]
		on: ReparseAfterSourceEditing
		do: [ :exception | self parseMethod: exception newSource onError: aBlock ]