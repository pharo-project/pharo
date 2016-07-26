submorphs-accessing
findA: aClass
	"Return the first submorph of the receiver that is descended from the given class. Return nil if there is no such submorph. Clients of this code should always check for a nil return value so that the code will be robust if the user takes the morph apart."

	^self submorphs
		detect: [:p | p isKindOf: aClass]
		ifNone: [nil]