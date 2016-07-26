printing
configMethodOn: aStream for: aValue selector: selector cascading: cascading cascade: cascade indent: indent

	| valuePrintString |
	aValue == nil ifTrue: [ ^self ].
	cascading ifTrue: [ aStream cr; tab: indent ].
	valuePrintString := aValue value isSymbol
		ifTrue: [ '#' , aValue value asString printString ]
		ifFalse: [ aValue value printString ].
	aStream  nextPutAll: selector, valuePrintString.
	cascade ifTrue: [ aStream nextPut: $; ]