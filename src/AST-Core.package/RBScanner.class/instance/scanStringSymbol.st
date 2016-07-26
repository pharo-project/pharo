private-scanning
scanStringSymbol
	| literalToken |
	literalToken := self scanLiteralString.
	literalToken value: literalToken value asSymbol.
	^literalToken