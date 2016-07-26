printing
configMethodValueOn: aStream for: spec selector: selector last: last indent: indent
	| valuePrintString |
	spec == nil
		ifTrue: [ ^ self ].
	valuePrintString := spec value isSymbol
		ifTrue: [ '#' , spec value asString printString ]
		ifFalse: [ spec value printString ].
	aStream
		tab: indent;
		nextPutAll: 'spec ' , selector , ' ' , valuePrintString , '.'.
	last
		ifFalse: [ aStream cr ]