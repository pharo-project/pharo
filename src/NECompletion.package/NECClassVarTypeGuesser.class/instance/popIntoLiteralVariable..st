bytecode decoding
popIntoLiteralVariable: anAssociation 
	anAssociation key == variableName asSymbol 
		ifTrue: [ found := true ]
		ifFalse: [ self reset ]