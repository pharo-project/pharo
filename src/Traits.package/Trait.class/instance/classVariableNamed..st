class variables
classVariableNamed: aString
	"Answer the Class Variable"

	^self classPool associationAt: aString asSymbol ifAbsent: [self error: 'no such class var']