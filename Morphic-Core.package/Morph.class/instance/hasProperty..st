accessing - properties
hasProperty: aSymbol 
	"Answer whether the receiver has the property named aSymbol"
	extension ifNil: [^ false].
	^extension hasProperty: aSymbol