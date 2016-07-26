submorphs-accessing
findDeeplyA: aClass
	"Return a morph in the submorph tree of the receiver that is descended from the given class. Return nil if there is no such morph. Clients of this code should always check for a nil return value so that the code will be robust if the user takes the morph apart."

	^ (self allMorphs copyWithout: self)
		detect: [:p | p isKindOf: aClass]
		ifNone: [nil]