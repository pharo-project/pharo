reading
readFrom: aReader
	| numberOfTraits numberOfClasses numberOfExtensionMethods |
	version := aReader readInt.
	packageName := aReader readByteSymbol.

	numberOfTraits := aReader readInt.
	1 to: numberOfTraits do: [ :idx | traits add: (HETrait readFrom: aReader)].

	numberOfClasses := aReader readInt.
	1 to: numberOfClasses do: [ :idx | classes add: (HEClass readFrom: aReader)].

	numberOfExtensionMethods := aReader readInt.
	1 to: numberOfExtensionMethods do: [ :idx | extensionMethods add: (HEMethod readFrom: aReader) ]