reading
doReadHeaderFrom: aReader
	className := aReader readByteSymbol.
	superclass := aReader readByteSymbol.
	instanceVariables := aReader readByteString.
	classInstancevariables := aReader readByteString.
	classVariables := aReader readByteString.
	sharedPools := aReader readByteString.
	layoutClass := aReader readByteSymbol