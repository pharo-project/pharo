accessing
fontOf: aCharacter
	"Answer the actual font to use for aCharacter"
	^self fontOf: aCharacter ifAbsent:[fontArray at: 1]