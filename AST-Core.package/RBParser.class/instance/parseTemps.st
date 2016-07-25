private-parsing
parseTemps
	| temps |
	temps := OrderedCollection new.
	[currentToken isIdentifier] whileTrue: [temps add: self parsePrimitiveIdentifier].
	^temps