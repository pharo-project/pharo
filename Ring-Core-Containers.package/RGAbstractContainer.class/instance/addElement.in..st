adding/removing
addElement: anObject in: aCollection

	aCollection isDictionary
		ifTrue: [ aCollection at: anObject fullName put: anObject ]
		ifFalse:[ aCollection add: anObject ]