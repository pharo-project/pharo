adding
addAllFirstUnlessAlreadyPresent: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the receiver, preserving the order, but do not add any items that are already in the receiver.  Answer anOrderedCollection."

	anOrderedCollection reverseDo:
		[:each | (self includes: each) ifFalse: [self addFirst: each]].
	^ anOrderedCollection