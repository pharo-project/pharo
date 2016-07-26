adding/removing
removeElement: anObject from: aCollection

	aCollection isDictionary
		ifTrue: [ aCollection removeKey: anObject fullName ifAbsent:[ ] ]
		ifFalse:[ aCollection remove: anObject ifAbsent:[ ] ]