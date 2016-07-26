adding
add: anObject
	"Include newObject as one of the receiver's elements. Answer newObject."
	tally = array size ifTrue:[self grow].
	array at: (tally := tally + 1) put: anObject.
	self updateObjectIndex: tally.
	self upHeap: tally.
	^anObject