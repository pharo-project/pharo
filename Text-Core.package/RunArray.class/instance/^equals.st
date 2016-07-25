comparing
= otherArray 
	"Test if all my elements are equal to those of otherArray"

	self == otherArray ifTrue: [^true].
	self species == otherArray species ifFalse: [^ false].
	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]