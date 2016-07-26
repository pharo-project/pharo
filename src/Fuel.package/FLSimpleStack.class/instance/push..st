adding
push: anObject 
	"Adds a new object of any kind on top of the stack."

	array at: slotIndex put: anObject.
	array size = slotIndex ifTrue: [ self grow ].
	slotIndex := slotIndex + 1.
	^ anObject.