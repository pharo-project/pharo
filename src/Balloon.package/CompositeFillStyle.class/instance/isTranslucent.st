testing
isTranslucent
	"Answer whether all of the composited fill styles are transparent."

	^self fillStyles allSatisfy: [:fs | fs isTranslucent]