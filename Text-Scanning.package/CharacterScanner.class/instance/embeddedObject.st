stop conditions
embeddedObject
	pendingKernX := 0.
	text attributesAt: lastIndex do:[:attr| 
		attr anchoredMorph ifNotNil:[
			"Try to placeEmbeddedObject: - if it answers false, then there's no place left"
			(self placeEmbeddedObject: attr anchoredMorph) ifFalse:[^self crossedX]]].
	"Note: if ever several objects are embedded on same character, only indent lastIndex once"
	lastIndex := lastIndex + 1.
	^false