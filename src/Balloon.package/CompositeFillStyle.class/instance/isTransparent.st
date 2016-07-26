testing
isTransparent
	"Answer whether all of the composited fill styles are transparent."

	^self fillStyles allSatisfy: [:fs | fs isTransparent]