operations
replaceAllWith: anotherTextModel	
	"Insert a copy of the argument at my start"

	| copy |
	copy := anotherTextModel veryDeepCopy.
	copy spansDo: [ :each | each text: self ].
	startSpan := copy startSpan.
	endSpan := startSpan last