removing
copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self copy
		remove: oldElement ifAbsent: [];
		yourself