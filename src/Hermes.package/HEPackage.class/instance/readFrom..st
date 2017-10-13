reading
readFrom: aReader
	| numberOfTraits numberOfClasses numberOfExtensionMethods |
	version := aReader readInt32.
	
	version = self class formatVersion ifFalse:[self error:'Invalid Hermes file version, expecting: ' , self class formatVersion asString , ' but was: ' , version asString].
	
	packageName := aReader readByteSymbol.

	numberOfTraits := aReader readInt32.
	1 to: numberOfTraits do: [ :idx | traits add: (HETrait readFrom: aReader)].

	numberOfClasses := aReader readInt32.
	1 to: numberOfClasses do: [ :idx | classes add: (HEClass readFrom: aReader)].

	numberOfExtensionMethods := aReader readInt32.
	1 to: numberOfExtensionMethods do: [ :idx | extensionMethods add: (HEMethod readFrom: aReader) ]  