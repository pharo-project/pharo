*Slot
isLegalInstVarName
	"Answer whether aString is a legal instance variable name."

	^ ((self isLiteralSymbol) and: [(self includes: $:) not]) and:
		[(self inviolateInstanceVariableNames includes:  self) not]