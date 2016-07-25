transforming
existingSelector
	"Try to find an existing method instead of creating a new one"

	^class allSelectors detect: [:each | self isMethodEquivalentTo: each]
		ifNone: [nil]