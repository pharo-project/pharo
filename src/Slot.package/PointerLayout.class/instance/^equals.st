comparing
= other
	^ super = other
		ifFalse: [  false ]
		ifTrue: [ self slotScope = other slotScope ]